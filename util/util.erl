-module(util).
-compile([export_all]).

do_with_statis(Label, Fun, Acc) ->
    do_with_statis(Label, Fun, Acc, 1).
do_with_statis(Label, Fun, Acc, N) ->
    erlang:garbage_collect(self()),
    {memory, MP1} = erlang:process_info(self(), memory),
    MT1 = erlang:memory(total),
    T1 = now(),
    {_, _} = erlang:statistics(runtime),
    {_, _} = erlang:statistics(wall_clock),
    {_, _} = erlang:statistics(reductions),
    Ret = do_times(Fun, Acc, N),
    T2 = now(),
    {_, RT} = erlang:statistics(runtime),
    {_, WT} = erlang:statistics(wall_clock),
    {_, Reds} = erlang:statistics(reductions),
    {memory, MP2} = erlang:process_info(self(), memory),
    MT2 = erlang:memory(total),
    NowT = timer:now_diff(T2, T1),
    io:format("==========~p==========\n", [Label]),
    io:format("return:~p ~p/sec\n", [Ret, N * 1000 div NowT]),
    io:format("reds:~p runtime:~p walktime:~p time:~p\n"
            "memory process:~p memory total:~p\n",
          [Reds, RT, WT, NowT, MP2 - MP1, MT2 - MT1]).

%% 执行多次
do_times(_F, Acc, 0) ->
    Acc;
do_times(F, Acc, N) ->
    Acc2 = (catch F(Acc)),
    do_times(F, Acc2, N - 1).
