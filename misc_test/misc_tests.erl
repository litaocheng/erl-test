-module(misc_tests).
-compile([export_all]).

run() ->
    div_vs_bsr(),
    list_2_atom(),
    test_now(),
    test_ets_lookup(),
    test_sets(),
    test_gb_sets(),
    ok.

%% div or bsr
div_vs_bsr() ->
    do_run("div", 100000, fun(N) -> N * 1000000 div 1000 end),
    do_run("bsr", 100000, fun(N) -> N * 1000000 bsr 10 end),
    ok.

%% now
test_now() ->
    do_run("now", 100000, fun(_N) -> erlang:now() end),
    ok.

%% test_ets_lookup
test_ets_lookup() ->
    Tid = ets:new(dummy, [set, public, {read_concurrency, true}]),
    {A, B, C} = Now = erlang:now(),
    ets:insert(Tid, {now, Now}),
    ets:insert(Tid, {now_ms, (A * 10000000 + B) * 1000 + C div 1000}),
    ets:insert(Tid, {now_sec, A * 10000000 + B}),
    do_run("ets_lookup", 100000, fun(_N) -> [_] = ets:lookup(Tid, now) end),
    ok.

%% test sets
test_sets() ->
    Set = sets:new(),
    do_run("test_sets", 100000, fun(N) ->


%%
list_2_atom() ->
    do_run("list_to_atom_1", 10000, 
        fun(N) -> erlang:list_to_atom(lists:concat(["redis_manager", "_cached_", N])) end),
    do_run("list_to_atom_2", 10000, 
        fun(N) -> erlang:list_to_atom("redis_manager" ++ "_cached_" ++ integer_to_list(N)) end),
    ok.

do_run(Title, N, Fun) ->
    T1 = now(),
    do_times(Fun, N),
    T2 = now(),
    T = timer:now_diff(T2, T1),
    io:format("================================~n"),
    io:format(" ~s~n", [Title]),
    io:format("================================~n"),
    io:format("total time:~p~n", [T]),
    io:format("avg time:~.2f (us)~n~n", [T/N]).

times(F, N) ->
    do_times(F, N).

do_times(_F, 0) ->
    ok;
do_times(F, N) ->
    F(N),
    do_times(F, N - 1).
