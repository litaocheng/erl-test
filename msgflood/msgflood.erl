%%%
%%% server start:
%%% $ erl -sname srv
%%% > msgflood:server(flood).
%%%
%%% client start:
%%% $ erl -sname client
%%% > msgflood:client(srv@litao, flood).
%%%
-module(msgflood).
-compile([export_all]).

%% 启动server
server(Server) ->
    true = is_alive(),
    register(Server, spawn_link(?MODULE, srv_loop, [self()])),
    io:format("Server ~p is running (from ~p)...~n", [Server, self()]),

    receive
        complete ->
            ok
    end,
    init:stop().

srv_loop(Parent) ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', _, shutdown} ->
            ok; 
        _Msg -> % first msg
            T1 = now(),
            N = srv_loop1(1),
            T2 = now(),
            T = timer:now_diff(T2, T1),
            io:format("receive msg : ~p~ntime:~p (micro sec)~nspeed:~p~n",
                [N, T, N * 1000000 / T]),
            Parent ! complete
    end.

srv_loop1(N) ->
    receive
        {'EXIT', _, shutdown} ->
            N; 
        stop ->
            N;
        {From, _Msg} ->
            From ! {ack, _Msg},
            srv_loop1(N+1)
    end.

%% 启动client(节点内)
client(Server) ->
    client(none, Server).

%% 启动client(节点间)
client(Node, Server) ->
    case Node of
        none ->
            Dest = Server;
        _ ->
            true = is_alive(),
            true = net_kernel:connect_node(Node),
            Dest = {Server, Node}
    end,
    Pid = spawn_link(?MODULE, send_loop, [self(), Dest]),
    io:get_line("press any key stop..."),
    exit(Pid, shutdown),
    receive
        complete ->
            ok
    after 5000 ->
            ok
    end,
    init:stop().
    
send_loop(Parent, Server) ->
    process_flag(trap_exit, true),
    T1 = now(),
    N = send_loop1(Server, 0),
    T2 = now(),
    T = timer:now_diff(T2, T1),
    io:format("send msg : ~p~ntime:~p (micro sec)~nspeed:~p~n",
        [N, T, N * 1000000 / T]),
    Parent ! complete.

send_loop1(Server, N) ->
    %Server ! <<"hello">>,
    Server ! {self(), <<"hello">>},
    receive
        {ack, _} ->
            send_loop1(Server, N+1);
        {'EXIT', _, shutdown} ->
            Server ! stop,
            N 
    after 0 ->
        send_loop1(Server, N+1)
    end.
