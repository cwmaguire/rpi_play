-module(pwm).

-export([go/0]).

-define(RED, 20).
-define(GREEN, 21).
-define(BLUE, 19).

go() ->
    Red = spawn(fun() -> cycle(?RED, 5, 0) end),
    Green = spawn(fun() -> cycle(?GREEN, 4, 0) end),
    Blue = spawn(fun() -> cycle(?BLUE, 3, 0) end),
    {stop_fun(Red, Green, Blue), {Red, Green, Blue}}.

stop_fun(Red, Green, Blue) ->
    fun() ->
         Red ! stop,
         Green ! stop,
         Blue ! stop
    end.

cycle(Pin, Step, TooHigh) when TooHigh + Step > 255 ->
    cycle(Pin, Step, 0);
cycle(Pin, Step, Current) ->
    rpi_play:pwm(Pin, Step),
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
            cycle(Pin, Step, Current + Step)
    end.
