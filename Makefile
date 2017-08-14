PROJECT = rpi_play
PROJECT_DESCRIPTION = Using epigpio to play with Raspberry Pi GPIO ports
PROJECT_VERSION = 0.1.0

DEPS = epigpio
dep_epigpio = git https://github.com/cwmaguire/epigpio.git

include erlang.mk
