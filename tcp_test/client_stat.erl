-module(client_stat).

-export([run/0]).

-include("tcp_test.hrl").
-define(SERVER, "127.0.0.1").
-define(RECV_TIMEOUT, 100).

run() ->
    N = 100000,
    C = 200,
    Per = N div C,
    Server = ?SERVER,
    _ = erlang:statistics(runtime),
    Procs = [new_client(self(), Server, Per) || _ <- lists:seq(1, C)],
    {Satis0, Timeout} = 
    lists:foldl(
        fun(_E, {SAcc, Timeout}) ->
            receive
                {S, T} ->
                    {[S | SAcc], T + Timeout}
            end
        end,
        {[], 0},
        Procs),
    {_, T} = erlang:statistics(runtime),
    {Satis, NotSatis} = 
    lists:partition(
        fun(E) when E =< 10000 ->
                true;
           (_) ->
                false
        end,
        lists:flatten(Satis0)),
    ?P("request count: ~p use time:~p(ms) speed:~p~n", [N, T, N * 1000 div T]),
    ?P("timeout: ~p statify: ~p~n", [Timeout + length(NotSatis), length(Satis)]),
    ?P("rsp time(us):~n", []),
    ?P("min:~p max:~p avg:~p~n", [lists:min(Satis),
                                  lists:max(Satis),
                                  lists:sum(Satis) div N
                              ]),
    erlang:halt(0).


new_client(Parent, Server, N) ->
    F = fun() ->
            random_init(),
            {ok, Sock} = connect_server(Server),
            timer:sleep(random:uniform(1000)),
            {ok, Statics} = client_loop(Sock, N, {[], 0}),
            Parent ! Statics
        end,
    spawn_link(F).

random_init() ->
    {A, B, C} = erlang:now(),
    random:seed(A, B, C).

connect_server(Server) ->
    case gen_tcp:connect(Server, ?PORT, [binary, {packet, 2}, {active, false}]) of
        {ok, S} ->
            {ok, S};
        {error, R} ->
            ?P("connect server error! ~p~n", [R]),
            {error, R}
    end.

client_loop(Sock, 0, Acc) ->
    {ok, Acc};
client_loop(Sock, N, {Statify, Timeout}) ->
    T1 = now(),
    ok = gen_tcp:send(Sock, <<"get">>),
    case gen_tcp:recv(Sock, 0, ?RECV_TIMEOUT) of
        {error, timeout} ->
            %?P("recv data timeout!~n", []),
            client_loop(Sock, N - 1, {Statify, Timeout + 1});
        {ok, Packet} ->
            T2 = now(),
            %?P("recv ~p~n", [Packet]),
            timer:sleep(10),
            client_loop(Sock, N - 1, {[timer:now_diff(T2, T1) | Statify], Timeout});
        {error, closed} ->
            ?P("socket closed!~p~n", [Sock]),
            ok
    end.

stop(Procs) ->
    [exit(Pid, kill) || Pid <- Procs].
