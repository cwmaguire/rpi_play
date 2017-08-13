# rpi_play
Controlling GPIO pins on the Raspberry Pi 3 from Erlang with epigpio

## Steps

I'm just going to list out all the hurdles I ran into getting this to
work with the default NOOBS-installed Raspbian

- Install the dev libraries for ssh and ssl:
  > apt-get install libssh-dev libssl-dev
- Install kerl
- Install Erlang/OTP 20.0 with kerl
  > kerl build 20.0 20.0
  > kerl install 20.0 /home/pi/dev/erlang/
