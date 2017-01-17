%%%-------------------------------------------------------------------
%% @doc epigpio public API
%% @end
%%%-------------------------------------------------------------------

-module(epigpio_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/1, stop/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    start(normal, []).

start([PIGPIO_IP, PIGPIO_PORT]) ->
  ok = application:set_env(epigpio, pigpiod_port, PIGPIO_PORT),
  ok = application:set_env(epigpio, pigpiod_ip, PIGPIO_IP),
  start(normal, []).

start(_StartType, []) ->
    case epigpio_app_sup:start_link() of
        {ok, AppSupPid} ->
            lager:debug("-------> Started epigpio, AppSupPid:  ~p~n",[AppSupPid]),
             application:set_env(epigpio, app_supervisor, AppSupPid),
            {ok, AppSupPid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
stop() ->
  stop([]).


stop(_State) ->
    lager:debug("-------> Error starting epigpio !!!~n"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
