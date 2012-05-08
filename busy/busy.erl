%% 让系统忙碌
-module(busy).
-compile([export_all]).

run() ->
    Parent = spawn(fun() -> do_mgr([]) end),
    do_wait_io(Parent).

%% 管理器
do_mgr(L) ->
    io:format("~w\n", [length(L)]),
    receive 
        add ->
            Pid = spawn(fun() -> do_work(0) end),
            do_mgr([Pid | L]);
        delete ->
            case L of
                [] ->
                    do_mgr([]);
                [H|T] ->
                    exit(H, kill),
                    do_mgr(T)
            end;
        _ ->
            do_mgr(L)
    end.
            
%% 等待用户输入
do_wait_io(Parent) ->
    Data = io:get_chars("press +/- to add/delete process > ", 1),
    case Data of
        "+" ->
            Parent ! add;
        "-" ->
            Parent ! delete;
        _ ->
            ok
    end,
    do_wait_io(Parent).
            
%% 工作进程
do_work(N) ->
    N11 = N * 100000000000000000000000 + N div 124234123421342143,
    N12 = round(N11 * 22223412341243142342),
    N2 = erlang:max(10000000, N12 + 1),
    timer:sleep(1),
    do_work(N2).
