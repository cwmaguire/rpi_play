-module(pwm).

-export([go/0]).

go() ->
    rpi_play:set_output_mode(10),
    rpi_play:pwm(10, 10000).
