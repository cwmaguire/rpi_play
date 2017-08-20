# rpi_play
Controlling GPIO pins on the Raspberry Pi 3 from Erlang with epigpio

- pwm.erl - Uses three PCM GPIO pins to control an RGB LED.
- lights.erl - Uses regular GPIO pins to control three regular LEDs
- rpi_play.erl - a genserver that that communicates with epigpio

So, the only use for rpi_play.erl is to receive messages that come back
from epigpio; so far these messages aren't really necessary.

## Hurdles

Hurdles I ran into getting this to work with the default NOOBS-installed Raspbian

- Install the dev libraries for ssh and ssl:
  > apt-get install libssh-dev libssl-dev
- Install the dev libraries for curses:
  > apt-get install libncurses5-dev libncursesw5-dev
- Install kerl
- Install Erlang/OTP 20.0 with kerl
  > kerl build 20.0 20.0

  > kerl install 20.0 /home/pi/dev/erlang/
