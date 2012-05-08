%% 判断throw对性能的影响
-module(throw).
-compile([export_all]).

-define(IF(Exp, T, F), case (Exp) of true -> (T); false -> (F) end).
-define(DO_STATIS(Exp),
    begin
        erlang:garbage_collect(self()),
        {memory, MP1} = erlang:process_info(self(), memory),
        MT1 = erlang:memory(total),
        T1 = now(),
        {_, _} = erlang:statistics(runtime),
        {_, _} = erlang:statistics(wall_clock),
        {_, _} = erlang:statistics(reductions),
        Exp,
        {_, RT} = erlang:statistics(runtime),
        {_, WT} = erlang:statistics(wall_clock),
        {_, Reds} = erlang:statistics(reductions),
        T2 = now(),
        {memory, MP2} = erlang:process_info(self(), memory),
        MT2 = erlang:memory(total),
        io:format("reds:~p runtime:~p walktime:~p time:~p memory process:~p memory total:~p\n",
                  [Reds, RT, WT, timer:now_diff(T2, T1), MP2 - MP1, MT2 - MT1]),
        ok
    end).


%% 执行操作
run(true) ->
    do_with_throw();
run(_) ->
    do_no_throw().

%% 执行throw的函数
do_with_throw() ->
    F = 
    fun({ok, Acc}) ->
        Acc2 = 1 + Acc,
        ?IF(is_atom(Acc2), throw({error, Acc2}), ok),
        {ok, Acc2}
    end,
    ?DO_STATIS({ok, _} = do_times(F, {ok, 0}, 100000)).

%% 执行没有throw函数
do_no_throw() ->
    F = 
    fun({ok, Acc}) ->
        Acc2 = 1 + Acc,
        {ok, Acc2}
    end,
    ?DO_STATIS({ok, _} = do_times(F, {ok, 0}, 100000)).

%% 执行多次
do_times(_F, Acc, 0) ->
    Acc;
do_times(F, Acc, N) ->
    Acc2 = (catch F(Acc)),
    do_times(F, Acc2, N - 1).
