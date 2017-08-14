-module(rpi_play).

-behaviour(gen_server).

%%% Communicate with epigpio, which will in turn communicate with pigpio (written in C)
%%% to play with GPIO pins on the Raspberry Pi.

%% API
-export([start_link/0]).
-export([set_output_mode/0]).
-export([power_on/0]).
-export([power_off/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {epigpio :: pid()}).

set_output_mode() ->
    gen_server:cast(?MODULE, set_output_mode).

power_on() ->
    gen_server:cast(?MODULE, power_on).

power_off() ->
    gen_server:cast(?MODULE, power_off).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Epigpio} = epigpio_api:start_pigpio_client(self()),
    {ok, #state{epigpio = Epigpio}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(set_output_mode, State = #state{epigpio = Epigpio}) ->
    epigpio:set_mode(Epigpio, 4, 1),
    {noreply, State};
handle_cast(power_on, State = #state{epigpio = Epigpio}) ->
    epigpio:write(Epigpio, 4, 1),
    {noreply, State};
handle_cast(power_off, State = #state{epigpio = Epigpio}) ->
    epigpio:write(Epigpio, 4, 0),
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
