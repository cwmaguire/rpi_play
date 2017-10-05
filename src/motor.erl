-module(motor).

-export([go/0]).

go() ->
    rpi_play:power_off(5),
    rpi_play:power_off(6),
    rpi_play:power_off(26),
    rpi_play:power_off(12),

    Steps = [fun step_1/0,
             fun step_2/0,
             fun step_3/0,
             fun step_4/0,
             fun step_5/0,
             fun step_6/0,
             fun step_7/0,
             fun step_8/0],

    [io:format("X: ~p, Y: ~p~n", [X, Y]) || Y <- lists:seq(1,6), X <- Steps],
    [step(X) || _ <- lists:seq(1,200), X <- Steps],

    rpi_play:power_off(5),
    rpi_play:power_off(6),
    rpi_play:power_off(26),
    rpi_play:power_off(12).

step(Fun) ->
    Fun(),
    timer:sleep(1).

step_1() ->
    io:format("Step 1~n"),
    rpi_play:power_off(5),
    rpi_play:power_off(6),
    rpi_play:power_off(26),
    rpi_play:power_on(12).

step_2() ->
    io:format("Step 2~n"),
    rpi_play:power_off(5),
    rpi_play:power_off(6),
    rpi_play:power_on(26),
    rpi_play:power_on(12).

step_3() ->
    io:format("Step 3~n"),
    rpi_play:power_off(5),
    rpi_play:power_off(6),
    rpi_play:power_on(26),
    rpi_play:power_off(12).

step_4() ->
    io:format("Step 4~n"),
    rpi_play:power_off(5),
    rpi_play:power_on(6),
    rpi_play:power_on(26),
    rpi_play:power_off(12).

step_5() ->
    rpi_play:power_off(5),
    rpi_play:power_on(6),
    rpi_play:power_off(26),
    rpi_play:power_off(12).

step_6() ->
    rpi_play:power_on(5),
    rpi_play:power_on(6),
    rpi_play:power_off(26),
    rpi_play:power_off(12).

step_7() ->
    rpi_play:power_on(5),
    rpi_play:power_off(6),
    rpi_play:power_off(26),
    rpi_play:power_off(12).

step_8() ->
    rpi_play:power_on(5),
    rpi_play:power_off(6),
    rpi_play:power_off(26),
    rpi_play:power_on(12).
