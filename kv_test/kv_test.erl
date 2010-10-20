-module(kv_test).
-compile(export_all).

-record(args, {
        n = 10000,  % 总数
        s = 5000    % 步长
    }).

%% 帮助信息
help() ->
    io:format(
    "kv_test 测试各种key-value的性能\n"
    "kv_test [Options]\n"
    " -h    print this help information\n"
    " -n    the max data size volume\n"
    " -s    the test run step\n"
    "说明:\n"
    "I表示插入,L表示查询\n"
    "memp表示所在进程内存变化，memt表示节点内存变化\n"),

    init:stop(1).

run() ->
    Args = init:get_plain_arguments(),
    %io:format("args is ~p~n", [Args]),
    run(Args, #args{}).

run(["-h"|_], _Args) ->
    help();
run(["-n", NS | Rest], Args) ->
    N = list_to_integer(NS), 
    run(Rest, Args#args{n = N});
run(["-s", SS | Rest], Args) ->
    S = list_to_integer(SS),
    run(Rest, Args#args{s = S});
run([_|Rest], Args) ->
    run(Rest, Args);
run([], Args) ->
    args_valid(Args),
    do_all(Args).

%% 验证参数是否合法
args_valid(#args{n = N, s = S}) 
    when is_integer(N), N >= S ->
    true;
args_valid(_) ->
    false.

%% 打印标题
print_title(N) ->
    io:format("\nN=~p\t~30s\t~30s\n", [N, "insert", "lookup"]),
    io:format("~12s\t|time~10s\tmemp\tmemt|\ttime\tspeed\tmemp\tmemt~n", 
        ["type", "speed"]),
    io:format("~80..-s\n", ["-"]).

%% 打印统计结果
print_stats(Type, N, [{insert, TI, MPI, MTI}, {lookup, TL, MPL, MTL}]) ->
    io:format("~12s\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p~n",
        [Type, TI, N * 1000000 div TI, MPI, MTI, TL, N * 1000000 div TL,MPL, MTL]).

%% 执行操作
do_all(#args{n = N, s = S}) ->
    Times = N div S,
    Rem = N rem S,
    L1 = 
    lists:foldl(
        fun(I, Acc) ->
            [I * S | Acc]
        end,
    [], lists:seq(1, Times)),
    L2 = 
    case Rem =:= 0 of
        true ->
            L1;
        false ->
            [N | L1]
    end,
    L = lists:reverse(L2),
    io:format("L is ~p~n", [L]),
    [do_run(I) || I <- L].

        
do_run(N) ->
    erlang:process_flag(trap_exit, true),
    Pid = spawn_link(fun worker/0),
    print_title(N),
    do_test_type(Pid, list, list_test(), [], N),
    %do_test_type(Pid, orddict, orddict_test(), orddict:new(), N),
    do_test_type(Pid, gtrees, gb_trees_test(), gb_trees:empty(), N),
    do_test_type(Pid, dict, dict_test(), dict:new(), N),
    do_test_type(Pid, mnesia, mnesia_trans_test(), mnesia_trans_init(N), N),
    do_test_type(Pid, 'mnesia(N)', mnesia_non_trans_test(), mnesia_non_trans_init(N), N),
    do_test_type(Pid, ets, ets_test(), ets_init(), N),
    do_test_type(Pid, pdict, proc_dict_test(), null, N).

do_test_type(Pid, Type, Funs, Opaque, N) ->
    Pid ! {Funs, Opaque, N},
    receive
        {Pid, Stats} ->
            print_stats(Type, N, Stats);
        {'EXIT', Pid, _Reason} ->
            io:format("the worker pid run error!~n")
    end.

%%-----------------------------------------------------------------
%% 列表操作
list_test() ->
    {fun(I, List) -> [{I, I} | List] end,
        fun(I, List) -> {I, I} = lists:keyfind(I, 1, List) end}.

%% dict操作
dict_test() ->
    {fun(I, Dict) -> dict:store(I, I, Dict) end,
        fun(I, Dict) -> {ok, I} = dict:find(I, Dict) end}.

%% orddict 操作
orddict_test() ->
    {fun(I, Dict) -> orddict:store(I, I, Dict) end,
        fun(I, Dict) -> {ok, I} = orddict:find(I, Dict) end}.

%% gb_trees 操作
gb_trees_test() ->
    {fun(I, Tree) -> gb_trees:insert(I, I, Tree) end,
        fun(I, Tree) -> {value, I} = gb_trees:lookup(I, Tree) end}.

%% ets 操作
ets_init() ->
    ets:new(ets_kv_test, [set, public, {keypos, 1}]).
ets_test() ->
    {fun(I, Tid) -> ets:insert(Tid, {I, I}), Tid end,
        fun(I, Tid) -> [{I, I}] = ets:lookup(Tid, I) end}.

%% mnesia 操作
-record(mnesia_kv_test, {
        key,
        value}).

mnesia_trans_init(N) ->
    mnesia:start(),
    Tab = list_to_atom(lists:concat(["mnesia_kv_trans", N])),
    {atomic, ok} = mnesia:create_table(Tab, [{ram_copies, [node()]}, {type, set},
            {record_name, mnesia_kv_test},
            {attributes, record_info(fields, mnesia_kv_test)}]),
    Tab.
mnesia_trans_test() ->
    {fun(I, Tab) -> 
            F =
            fun() ->
                    ok = mnesia:write(Tab, #mnesia_kv_test{key = I, value = I}, write)
            end,
            {atomic, ok} = mnesia:transaction(F),
            Tab
    end,
    fun(I, Tab) -> 
            F =
            fun() ->
                    mnesia:dirty_read({Tab, I})
            end,
            {atomic, [#mnesia_kv_test{}]} = mnesia:transaction(F)
    end}.

mnesia_non_trans_init(N) ->
    mnesia:start(),
    Tab = list_to_atom(lists:concat(["mnesia_kv_non_trans", N])),
    {atomic, ok} = mnesia:create_table(Tab, [{ram_copies, [node()]}, {type, set},
            {record_name, mnesia_kv_test},
            {attributes, record_info(fields, mnesia_kv_test)}]),
    Tab.
mnesia_non_trans_test() ->
    {fun(I, Tab) -> 
            ok = mnesia:dirty_write(Tab, #mnesia_kv_test{key = I, value = I}),
            Tab
    end,
    fun(I, Tab) -> 
            [#mnesia_kv_test{}] = mnesia:dirty_read({Tab, I})
    end}.

%% Process dictionary 操作
proc_dict_test() ->
    {fun(I, _) -> put(I, I) end,
        fun(I, _) -> I = get(I) end}.

%%-----------------------------------------------------------------

%% 使用一个独立process，首先进行插入，随后进行查询。
worker() ->
    receive
        {{FunInsert, FunLookup}, Opaque, N} ->
            erase(),
            erlang:garbage_collect(),
            {Opaque2, S1} = do_insert(FunInsert, Opaque, N),
            S2 = do_lookup(FunLookup, Opaque2, N),
            {links, [Parent]} = erlang:process_info(self(), links),
            Parent ! {self(), [S1, S2]},
            worker()
    end.

do_insert(Fun, Opaque, N) ->
    {memory, MP1} = erlang:process_info(self(), memory),
    MT1 = erlang:memory(total),
    T1 = now(),
    Opaque2 = do_times(Fun, Opaque, N, true),
    T2 = now(),
    {memory, MP2} = erlang:process_info(self(), memory),
    MT2 = erlang:memory(total),
    {Opaque2, {insert, timer:now_diff(T2, T1), MP2 - MP1, MT2 - MT1}}.

do_lookup(Fun, Opaque, N) ->
    {memory, MP1} = erlang:process_info(self(), memory),
    MT1 = erlang:memory(total),
    T1 = now(),
    do_times(Fun, Opaque, N, false),
    T2 = now(),
    {memory, MP2} = erlang:process_info(self(), memory),
    MT2 = erlang:memory(total),
    {lookup, timer:now_diff(T2, T1), MP2 - MP1, MT2 - MT1}.

%% 某个动作执行多次
do_times(_Fun, Opaque, 0, _IsAcc) ->
    Opaque;
do_times(Fun, Opaque, N, IsAcc) ->
    Opaqua2 =
    case IsAcc of
        true ->
            Fun(N, Opaque);
        false ->
            Fun(N, Opaque),
            Opaque
    end,
    do_times(Fun, Opaqua2, N-1, IsAcc).
