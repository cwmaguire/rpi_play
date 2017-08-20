-module(rpi_play).

-behaviour(gen_server).

%%% Communicate with epigpio, which will in turn communicate with pigpio (written in C)
%%% to play with GPIO pins on the Raspberry Pi.

%% API
-export([start_link/0]).
-export([set_output_mode/1]).
-export([power_on/1]).
-export([power_off/1]).
-export([pwm/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {epigpio :: pid()}).

set_output_mode(Pin) ->
    gen_server:cast(?MODULE, {set_output_mode, Pin}).

power_on(Pin) ->
    gen_server:cast(?MODULE, {power_on, Pin}).

power_off(Pin) ->
    gen_server:cast(?MODULE, {power_off, Pin}).

pwm(Pin, DutyCycle) ->
    gen_server:cast(?MODULE, {pwm, Pin, DutyCycle}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Epigpio} = epigpio_api:start_pigpio(self()),
    {ok, #state{epigpio = Epigpio}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({set_output_mode, Pin}, State = #state{epigpio = Epigpio}) ->
    epigpio:set_mode(Epigpio, Pin, 1),
    {noreply, State};
handle_cast({power_on, Pin}, State = #state{epigpio = Epigpio}) ->
    epigpio:write(Epigpio, Pin, 1),
    {noreply, State};
handle_cast({power_off, Pin}, State = #state{epigpio = Epigpio}) ->
    epigpio:write(Epigpio, Pin, 0),
    {noreply, State};
handle_cast({pwm, Pin, DutyCycle}, State = #state{epigpio = Epigpio}) ->
    epigpio:pwm(Epigpio, Pin, DutyCycle),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Message from epigpio: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
