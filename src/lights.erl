-module(lights).

-export([go/0]).

go() ->
    rpi_play:power_on(4),
    timer:sleep(50),
    rpi_play:power_off(4),
    rpi_play:power_on(27),
    timer:sleep(50),
    rpi_play:power_off(27),
    rpi_play:power_on(17),
    timer:sleep(50),
    rpi_play:power_off(17).

