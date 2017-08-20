-module(pwm).

-export([go/0]).

-define(RED, 20).
-define(GREEN, 21).
-define(BLUE, 19).

go() ->
    go([{?RED, 5, 100},
        {?GREEN, 4, 100},
        {?BLUE, 3, 100}]).

go(Specs) ->
    [rpi_play:set_output_mode(Pin) || {Pin, _, _} <- Specs],
    Pids = [spawn(fun() -> cycle(P, St, Sl, 0) end) || {P, St, Sl} <- Specs],
    %Red = spawn(fun() -> cycle(?RED, 5, 1000, 0) end),
    %Green = spawn(fun() -> cycle(?GREEN, 4, 500, 0) end),
    %Blue = spawn(fun() -> cycle(?BLUE, 3, 2000, 0) end),
    {stop_fun(Pids), Pids}.

stop_fun(Pids) ->
    fun() ->
         [Pid ! stop || Pid <- Pids]
    end.

cycle(Pin, Step, Sleep, TooHigh) when TooHigh + Step > 255 ->
    cycle(Pin, Step, Sleep, 0);
cycle(Pin, Step, Sleep, Current) ->
    rpi_play:pwm(Pin, Current),
    ShouldContinue =
    receive
        stop ->
            false;
        Msg ->
            io:format("~p (pin ~p) receive message: ~p~n", [self(), Pin, Msg]),
            true
    after 0 ->
        true
    end,
    case ShouldContinue of
        false ->
            okay;
        _ ->
            timer:sleep(Sleep),
            cycle(Pin, Step, Sleep, Current + Step)
    end.
