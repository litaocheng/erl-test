-module(timer_test).
-compile([export_all]).

-define(TIMES, 100000).
-define(INTERVAL, 100).
-define(EVENT, 'timer_test_event').
run() ->
    Parent = self(),
    run_1(Parent).

run_1(Parent) ->
    L = lists:seq(1, ?TIMES),
    F = 
    fun() ->
        erlang:start_timer(?INTERVAL, self(), {?EVENT, now()}),
        receive
            {timeout, _, {?EVENT, Prev}} ->
                Now = erlang:now(),
                Ms = timer:now_diff(Now, Prev) div 1000,
                Parent ! {result, Ms}
        end
    end,
    Pids = [spawn(F) || _ <- L],
    [begin
        receive
            {result, Ms} ->
                add_statis(Ms)
        end
    end || _ <- Pids],
    Statis = get_statis(),
    clear_statis(),
    calc_statis(Statis).

%%----------
%% statis
%%----------
-define(STATIS, statis).

%% statis
add_statis(Dev) -> 
    L = get_statis(),
    erlang:put(?STATIS, [Dev | L]),
    ok.

clear_statis() ->
    erlang:erase(?STATIS),
    ok.

get_statis() ->
    case erlang:get(?STATIS) of
        undefined ->
            [];
        L ->
            L
    end.
    
calc_statis(L) ->
    Len = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Avg = lists:sum(L) div Len,
    io:format("min ~p max ~p avg ~p\n", [Min, Max, Avg]),
    L2 = do_split(L),
    io:format("deviation       number\n"),
    io:format("=========    ============\n"),
    [begin
    io:format("~9.b    ~5.b(~.1f%)\n", [K, V, V * 100 / Len])
    end || {K, V} <- L2],
    ok.
    

do_split(L) ->
    Dict = do_split(L, dict:new()),
    lists:sort(dict:to_list(Dict)).

do_split([], Acc) ->
    Acc;
do_split([H|T], Acc) ->
    Dev = erlang:abs(H - ?INTERVAL),
    Acc2 = dict:update_counter(Dev, 1, Acc),
    do_split(T, Acc2).

