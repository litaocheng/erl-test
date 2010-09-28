-module(tcp_server).
-compile(export_all).
-include("tcp_test.hrl").

start() ->
    start(?PORT).
start(Port) ->
    N = erlang:system_info(schedulers),
    listen(Port, N),
    io:format("tcp_server ready with ~b schedulers on port ~b~n", [N, Port]),

    register(?MODULE, self()),
    receive Any -> io:format("~p~n", [Any]) end.  %% to stop: tcp_server!stop.

listen(Port, N) ->
    Opts = [{active, false},
            binary,
            {backlog, 256},
            {packet, 2},
            %%{raw,6,9,<<1:32/native>>}, %defer accept
            %%{delay_send,true},
            {nodelay, false},
            {reuseaddr, true}],

    {ok, S} = gen_tcp:listen(Port, Opts),
    Spawn = fun(I) ->     
		    register(list_to_atom("acceptor_" ++ integer_to_list(I)),
			     spawn_opt(?MODULE, accept, [S, I], [link, {scheduler, I}]))
    end,
    lists:foreach(Spawn, lists:seq(1, N)).

accept(S, I) ->
    case gen_tcp:accept(S) of
        {ok, Socket} -> spawn_opt(?MODULE, loop, [Socket], [{scheduler, I}]);
        Error    -> erlang:error(Error)
    end,
    accept(S, I).

loop(S) ->
    case gen_tcp:recv(S, 0) of
        {ok, <<"get">>} ->
        %{ok, Packet} ->
            %?P("get packet ~p~n", [Packet]),
            %?P(".", []),
            {A,B,C} = now(),
            random:seed(A, B, C),
            N = random:uniform(100000), 
            Response = integer_to_list(N),
            gen_tcp:send(S, Response),
            %gen_tcp:close(S),
            loop(S);
        Error ->
            Error
    end.

