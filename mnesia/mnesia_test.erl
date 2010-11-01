-module(mnesia_test).
-compile(export_all).

%% 关于数据加载测试参数
-record(load_args, {
        n = 100000
    }).

%% 关于性能测试参数
-record(perf_args, {
        type = ram_copies,  % mensia table types
        n = 100000,              % 数据表行数
        r = 1000,              % 读取次数
        w = 1000,              % 写入次数
        index = []      % 添加索引字段
    }).

-record(person, {
        name,
        sex,
        age,
        score,
        desc
    }).

-define(S2N(L), (list_to_integer(L))).

%% 帮助信息
help() ->
    io:format(
    "mnesia_test 测试mnesia的性能\n"
    "mnesia_test Command [Options]\n"
    "Command:\n"
    " -h    显示帮助信息\n"
    " load  测试数据的加载速度\n"
    "   -n    数据行数\n"
    "\n"
    " perf  测试读取写入速度\n"
    "   -n    数据行数\n"
    "   -r    读取次数\n"
    "   -w    写入次数\n"
    "   -t    mnesia表类型:\n"
    "         ram 内存\n"
    "         disc 磁盘表（会包含一个内存映像)\n"
    "         disc_only 仅包含磁盘表\n"
    "   -i    为某个字段添加索引:\n"
    "         sex 性别字段\n"
    "         age 年龄字段\n"
    "         all 上面提到的所有字段\n"
    "\n"),

    init:stop(1).

%% 执行测试
run() ->
    PArgs = init:get_plain_arguments(),
    Args = parse_args(PArgs),
    do_test(Args).

%% 解析参数
parse_args([]) ->
    help();
parse_args(["-h"|_]) ->
    help();
parse_args(["load" | Rest]) ->
    parse_load_args(Rest, #load_args{});
parse_args(["perf" | Rest]) ->
    parse_perf_args(Rest, #perf_args{}).

%% 解析load参数
parse_load_args(["-n", NStr | Rest], Args) ->
    parse_load_args(Rest, Args#load_args{n = ?S2N(NStr)});
parse_load_args([_|Rest], Args) ->
    parse_load_args(Rest, Args);
parse_load_args([], Args) ->
    Args.

%% 解析perf参数
parse_perf_args(["-n", NStr | Rest], Args) ->
    parse_perf_args(Rest, Args#perf_args{n = ?S2N(NStr)});
parse_perf_args(["-r", NStr | Rest], Args) ->
    parse_perf_args(Rest, Args#perf_args{r = ?S2N(NStr)});
parse_perf_args(["-w", NStr | Rest], Args) ->
    parse_perf_args(Rest, Args#perf_args{w = ?S2N(NStr)});
parse_perf_args(["-t", Type | Rest], Args) ->
    TableType =
    case Type of
        "ram" ->
            ram_copies;
        "disc" ->
            disc_copies;
        "disc_only" ->
            disc_only_copies;
        _ ->
            help()
    end,
    parse_perf_args(Rest, Args#perf_args{type = TableType});
parse_perf_args(["-i", IndexStr | Rest], Args) ->
    Index =
    case IndexStr of
        "sex" ->
            [#person.sex];
        "age" ->
            [#person.age];
        "all" ->
            [#person.sex, #person.age];
        _ ->
            help()
    end,
    parse_perf_args(Rest, Args#perf_args{index = Index});
parse_perf_args([_|Rest], Args) ->
    parse_perf_args(Rest, Args);
parse_perf_args([], Args) ->
    Args.

%% 执行具体的动作
do_test(_Args = #load_args{n = N}) ->
    io:format("args is ~p\n", [_Args]),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    makesure_use_dir(),
    {atomic, ok} = 
    mnesia:create_table(person, [
                                {disc_copies, [node()]},
                                {attributes, record_info(fields, person)},
                                {load_order, 1},
                                {type, set}
                            ]),

    % 插入数据
    [begin 
            ok = mnesia:dirty_write(random_person(I))
    end || I <- lists:seq(1, N)],
    %show_info(),
    % 停止mnesia
    stopped = mnesia:stop(),
    T1 = erlang:now(),
    ok = mnesia:start(),
    mnesia:wait_for_tables([person], infinity),
    T2 = erlang:now(),
    T = timer:now_diff(T2, T1),
    show_info(),
    io:format("load table(~p) time:~p speed:~p/s\n", [N, T, N*1000000 div T]);
do_test(_Args = #perf_args{type = Type, n = N, r = Read, w = Write, index = Index}) ->
    io:format("args is ~p\n", [_Args]),
    case erlang:is_alive() of
        false ->
            net_kernel:start([t1, shortnames]);
        true ->
            ok
    end,
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    case Type of
        ram_copies ->
            ok;
        Type when Type =:= disc_copies; Type =:= disc_only_copies ->
            makesure_use_dir()
    end,

    {atomic, ok} = 
    mnesia:create_table(person, [
                                {index, Index},
                                {Type, [node()]},
                                {attributes, record_info(fields, person)},
                                {load_order, 1},
                                {type, set}
                            ]),

    % 插入数据
    [begin 
            ok = mnesia:dirty_write(random_person(I))
    end || I <- lists:seq(1, N)],
    % 显示表信息
    %show_info(),

    % 生成随机Keys
    ReadKeys = [random:uniform(N) || _ <- lists:duplicate(Read, 1)],
    WriteKeys = [random_person(random:uniform(N)) || _ <- lists:duplicate(Write, 1)],

    % 同时进行读取和写入
    Parent = self(),
    {ok, Pid1} = start_reader(Parent, ReadKeys),
    {ok, Pid2} = start_writer(Parent, WriteKeys),
    [begin
        receive
            ack ->
                ok
        end
    end || _P <- [Pid1, Pid2]].


%% 启动一个读进程
start_reader(Parent, Keys) ->
    N = length(Keys),
    FTrans =
    fun() ->
        mnesia:transaction(
        fun() ->
                [mnesia:read(person, Key) || Key <-Keys]
        end)
    end,

    FDirty =
    fun() ->
        [mnesia:dirty_read(person, Key) || Key <-Keys]
    end,

    FMatch =
    fun() ->
        [mnesia:dirty_match_object(#person{name = Key, _='_'}) || Key <- Keys]
    end,

    F =
    fun() ->
            % transaction read
            {T1, _} = timer:tc(FTrans, []),
            {T2, _} = timer:tc(FDirty, []),
            {T3, _} = timer:tc(FMatch, []),
            io:format(
                "read time:~p speed:~p\n"
                "dread time:~p speed:~p\n"
                "match time:~p speed:~p\n",
                [T1, N*1000000 div T1,
                T2, N*1000000 div T2,
                T3, N*1000000 div T3]),
            Parent ! ack
    end,
    {ok, spawn_link(F)}.

%% 写进程
start_writer(Parent, Objs) ->
    N = length(Objs),
    FTrans =
    fun() ->
        mnesia:transaction(
        fun() ->
                [ok = mnesia:write(O) || O <- Objs]
        end)
    end,

    FDirty =
    fun() ->
        [ok = mnesia:dirty_write(person, O) || O <- Objs]
    end,

    F =
    fun() ->
            % transaction read
            {T1, _} = timer:tc(FTrans, []),
            {T2, _} = timer:tc(FDirty, []),
            io:format(
                "write time:~p speed:~p\n"
                "dwrite time:~p speed:~p\n",
                [T1, N*1000000 div T1,
                T2, N*1000000 div T2
                ]),
            Parent ! ack
    end,
    {ok, spawn_link(F)}.

%% 随机生成一个人
random_person(N) ->
    #person{name = N, 
        sex = random:uniform(2), 
        age = random:uniform(100),
        score = random:uniform(750),
        desc = lists:concat(["my name is name_", N])
    }.

%% 显示信息
show_info() ->
    mnesia:info(),
    mnesia:system_info(all),
    All = mnesia:table_info(person, all),
    io:format("table:\n~p\n", [All]).

makesure_use_dir() ->
    case mnesia:table_info(schema, storage_type) of
        ram_copies ->
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        disc_copies ->
            ok
    end.
