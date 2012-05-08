-module(kv_test).
-compile(export_all).

-record(args, {
        n = 10000,  % 总数
        s = 5000,   % 步长
        types = []  % 要测试的类型
    }).

-define(CAN_RUN(N, TYPES, FUN), (case lists:member(N, TYPES) of true -> FUN; false -> ok end)).

%% 帮助信息
help() ->
    io:format(
"kv_test 测试各种key-value的性能
kv_test [Options] types
选项
 -h    print this help information
 -n    the max data size volume
 -s    the test run step

types:
 all表示所有
 1-list 2-orddict 3-gb_tree 4-dict 5-mnesia 6-mnesia(dirty)
 7-ets 8-pdict 9-mochiglobal 10-fmatch 11-sets 12-gb_sets 13-ordsets

说明:
I表示插入,L表示查询
memp表示所在进程内存变化，memt表示节点内存变化\n"),
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
run(["all"|Rest], Args) ->
    run(Rest, Args#args{types = lists:seq(1, 13)});
run([Type|Rest], #args{types = Old} = Args) ->
    run(Rest, Args#args{types = [list_to_integer(Type) | Old]});
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
    io:format("~12s\t\ttime~10s\tmemp\tmemt\t\ttime\tspeed\tmemp\tmemt~n", 
        ["type", "speed"]),
    io:format("~95..-s\n", ["-"]).

%% 打印统计结果
print_stats(Type, N, [{insert, TI, MPI, MTI}, {lookup, TL, MPL, MTL}]) ->
    io:format("~12s\t\t~p\t~p\t~p\t~p\t\t~p\t~p\t~p\t~p~n",
        [Type, TI, N * 1000000 div TI, MPI, MTI, TL, N * 1000000 div TL,MPL, MTL]).

%% 执行操作
do_all(#args{types = []}) ->
    help();
do_all(#args{n = N, s = S, types = Types}) ->
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
    [do_run(I, Types) || I <- L].

        
do_run(N, Types) ->
    erlang:process_flag(trap_exit, true),
    Pid = spawn_link(fun worker/0),
    print_title(N),
    ?CAN_RUN(1, Types, do_test_type(Pid, list, list_test(), [], N)),
    ?CAN_RUN(2, Types, do_test_type(Pid, orddict, orddict_test(), orddict:new(), N)),
    ?CAN_RUN(3, Types, do_test_type(Pid, gtrees, gb_trees_test(), gb_trees:empty(), N)),
    ?CAN_RUN(4, Types, do_test_type(Pid, dict, dict_test(), dict:new(), N)),
    ?CAN_RUN(5, Types, do_test_type(Pid, mnesia, mnesia_trans_test(), mnesia_trans_init(N), N)),
    ?CAN_RUN(6, Types, do_test_type(Pid, 'mnesia(N)', mnesia_non_trans_test(), mnesia_non_trans_init(N), N)),
    ?CAN_RUN(7, Types, do_test_type(Pid, ets, ets_test(), ets_init(), N)),
    ?CAN_RUN(8, Types, do_test_type(Pid, pdict, proc_dict_test(), null, N)),
    ?CAN_RUN(9, Types, do_test_type(Pid, mochiglobal, mochiglobal_test(), null, N)),
    ?CAN_RUN(10, Types, do_test_type(Pid, fmatch, fun_match_test(), fun_match_init(N), N)),
    ?CAN_RUN(11, Types, do_test_type(Pid, sets, sets_test(), sets:new(), N)),
    ?CAN_RUN(12, Types, do_test_type(Pid, gb_sets, gb_sets_test(), gb_sets:new(), N)),
    ?CAN_RUN(13, Types, do_test_type(Pid, ordsets, ordsets_test(), ordsets:new(), N)),
    ok.

do_test_type(Pid, Type, Funs, Opaque, N) ->
    Pid ! {Funs, Opaque, N},
    receive
        {Pid, Stats} ->
            print_stats(Type, N, Stats);
        {'EXIT', Pid, _Reason} ->
            io:format("the worker pid run error!~n")
    end.

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
    ets:new(ets_kv_test, [set, public, {keypos, 1}, {read_concurrency, true}]).
ets_test() ->
    {fun(I, Tid) -> ets:insert(Tid, {I, I*I}), Tid end,
        fun(I, Tid) -> [{I, _}] = ets:lookup(Tid, I) end}.

%% Process dictionary 操作
proc_dict_test() ->
    {fun(I, _) -> put(I, I) end,
        fun(I, _) -> I = get(I) end}.

%% mochiglobal 操作
mochiglobal_test() ->
    {fun(I, _) -> mochiglobal:put(I, I) end,
        fun(I, _) -> I = mochiglobal:get(I) end}.

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

fun_match_init(N) ->
    Mod = kv_test_fun,
    Def =
    io_lib:format(
    "-module(~w).\n"
    "-compile([export_all]).\n\n",
    [Mod]),

    Body = 
    [begin
        io_lib:format(
        "find(~b) ->\n"
        "   ~p;\n",
        [I, I])
    end || I <- lists:seq(1, N)],

    LastClause = "find(_) -> throw(unknown_key).\n",

    FileName = lists:concat([Mod, ".erl"]),
    ok = file:write_file(FileName, [Def,Body, LastClause]),
    {ok, Mod} = compile:file(FileName),
    Mod:module_info(),
    ok.


%% 采用函数匹配操作
fun_match_test() ->
    {fun(_I, _) -> ok end,
        fun(I, _) -> kv_test_fun:find(I) end}.


%% 测试sets
sets_test() ->
    {fun(I, Sets) -> sets:add_element(I, Sets) end,
        fun(I, Sets) -> true = sets:is_element(I, Sets) end}.

%% 测试gb_sets
gb_sets_test() ->
    {fun(I, Sets) -> gb_sets:add_element(I, Sets) end,
        fun(I, Sets) -> true = gb_sets:is_element(I, Sets) end}.

%% 测试ordsets
ordsets_test() ->
    {fun(I, Sets) -> ordsets:add_element(I, Sets) end,
        fun(I, Sets) -> true = ordsets:is_element(I, Sets) end}.

%%-------------
%% 执行逻辑
%%-------------

%% 使用一个独立process，首先进行插入，随后进行查询。
worker() ->
    receive
        {{FunInsert, FunLookup}, Opaque, N} ->
            erase(),
            erlang:garbage_collect(self()),
            {Opaque2, S1} = do_insert(FunInsert, Opaque, N),
            erlang:garbage_collect(self()),
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
