%%%-------------------------------------------------------------------
%% @doc epigpio top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(epigpio_app_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = brutal_kill,
  Type = worker,

  EpigpioAppSrv = {epigpio_app_srv, {epigpio_app_srv, start_link, [self()]},
    Restart, Shutdown, Type, [epigpio_app_srv]},
  {ok, {SupFlags, [EpigpioAppSrv]}}.


%%====================================================================
%% Internal functions
%%====================================================================
