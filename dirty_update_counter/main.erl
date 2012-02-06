-module(main).
-compile([export_all]).

-define(TABLE, ?MODULE).
-define(SERVER, ?MODULE).

%%--------
%% 服务器
%%--------
server() ->
    true = is_alive(),
    register(?SERVER, self()),
    do_server_init(),
    io:format("Server is running (~p)...~n", [self()]),
    receive
        finish ->
            ok
    end,
    init:stop().

do_server_init() ->
    mnesia:start(),
    {atomic, ok} =
    mnesia:create_table(?TABLE, [{type, set}, {ram_copies, [node()]}]),
    ok.

%%--------
%% 客户端
%%--------
%% 启动client
client(Node, Num) when is_integer(Num), Num > 0 ->
    true = is_alive(),
    true = net_kernel:connect_node(Node),
    ok = mnesia:start(),
    {ok, _} = mnesia:change_config(extra_db_nodes, [Node]),
    Seq = lists:seq(1, Num),
    T1 = erlang:now(),
    Pids = 
    [begin 
        spawn_link(?MODULE, client_loop, [self()])
    end || _ <- Seq],
    io:get_line("press any key stop..."),
    Result =
    [begin 
        exit(Pid, shutdown),
        receive
            {finish, N, Id} ->
                {N, Id}
        end
    end || Pid <- Pids],
    T2 = erlang:now(),
    {?SERVER, Node} ! finish,
    T = timer:now_diff(T2, T1),
    do_statis(T, Result),
    init:stop().
    
client_loop(Parent) ->
    process_flag(trap_exit, true),
    {N, Id} = do_client_loop(1),
    Parent ! {finish, N, Id}.

do_client_loop(N) ->
    Id = do_new_id(),
    receive
        {'EXIT', _, shutdown} ->
            {N, Id}
    after 0 ->
        do_client_loop(N+1)
    end.

do_new_id() ->
    case catch mnesia:dirty_update_counter(?TABLE, 'default', 1) of
        {'EXIT', _Reason} ->
            io:format("类型自增id失败:~p", [_Reason]),
            exit(_Reason); 
        Id ->
            Id  
    end.

%% 进行统计
do_statis(T, Result) ->
    {NL, IdL} = lists:unzip(Result),
    NSum = lists:sum(NL),
    io:format("用时:~p 次数:~p 速度:~p/s\n", [T, NSum, NSum * 1000000 div T]),
    io:format("最后id为:~p\n", [IdL]),
    ok.
