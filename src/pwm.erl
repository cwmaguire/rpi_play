-module(pwm).

-export([go/0]).

go() ->
    rpi_play:pwm(10, 10000).
