-module(misc_tests).
-compile([export_all]).

list_to_atom(Count) ->
    T1 = now(),
    do_times(fun(N) ->
        R = erlang:list_to_atom(lists:concat(["redis_manager", "_cached_", N])),
        ok
        %io:format("result is :~p~n", [R])
    end, Count),
    T2 = now(),
    T = timer:now_diff(T2, T1),
    io:format("list_to_atom:~n"),
    io:format("total time:~p~n", [T]),
    io:format("avg time:~.2f (us)~n", [T/Count]).

times(F, N) ->
    do_times(F, N).


do_times(_F, 0) ->
    ok;
do_times(F, N) ->
    F(N),
    do_times(F, N - 1).
