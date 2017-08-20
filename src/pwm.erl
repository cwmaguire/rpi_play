-module(pwm).

-export([go/0]).
-export([loop/0]).

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
    {stop_fun(Pids), Pids}.

loop() ->
    [rpi_play:set_output_mode(Pin) || Pin <- [?RED, ?GREEN, ?BLUE]],
    Pid = spawn(fun() ->
                       loop({{?RED, 0}, {?GREEN, 0}, {?BLUE, 0}}, 15, 40)
                end),
    {stop_fun([Pid]), Pid}.

stop_fun(Pids) ->
    fun() ->
         [Pid ! stop || Pid <- Pids]
    end.

loop({{Red, RCurrent}, Green, Blue}, Step, Sleep) when RCurrent + Step > 255 ->
    loop({{Red, 0}, Green, Blue}, Step, Sleep);
loop({{Red, RCurrent}, {Green, GCurrent}, Blue}, Step, Sleep) when GCurrent + Step > 255 ->
    loop({{Red, RCurrent + Step}, {Green, 0}, Blue}, Step, Sleep);
loop({Red, {Green, GCurrent}, {Blue, BCurrent}}, Step, Sleep) when BCurrent + Step > 255 ->
    loop({Red, {Green, GCurrent + Step}, {Blue, 0}}, Step, Sleep);
loop({R = {Red, RCurrent}, G = {Green, GCurrent}, {Blue, BCurrent}}, Step, Sleep) ->
    rpi_play:pwm(Red, RCurrent),
    rpi_play:pwm(Green, GCurrent),
    rpi_play:pwm(Blue, BCurrent),
    ShouldContinue =
    receive
        stop ->
            false;
        Msg ->
            io:format("~p receive message: ~p~n", [self(), Msg]),
            true
    after 0 ->
        true
    end,
    case ShouldContinue of
        false ->
            okay;
        _ ->
            timer:sleep(Sleep),
            loop({R, G, {Blue, BCurrent + Step}}, Step, Sleep)
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
