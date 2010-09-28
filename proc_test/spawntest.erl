-module(spawntest).
-export([serial_spawn/1]).

serial_spawn(M) ->
    N = 1000000,
    NpM = N div M,
    %Start = erlang:now(),
    {_, _} = erlang:statistics(wall_clock),
    dotimes(M, fun () -> serial_spawn(self(), NpM) end),
    dotimes(M, fun () -> receive X -> X end end),
    %Stop = erlang:now(),
    {_, T} = erlang:statistics(wall_clock),
    %round((NpM * M) * 1000000 / timer:now_diff(Stop, Start)).
    round((NpM * M) * 1000 / T).

serial_spawn(Who, 0) -> Who ! done;
serial_spawn(Who, Count) ->
    spawn(fun () ->
        serial_spawn(Who, Count - 1)
    end).

dotimes(0, _) -> done;
dotimes(N, F) ->
    F(),
    dotimes(N - 1, F).
