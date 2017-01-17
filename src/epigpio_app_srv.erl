%%%-------------------------------------------------------------------
%%% @author vasco
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 22:49
%%%-------------------------------------------------------------------
-module(epigpio_app_srv).
-author("vasco").

-behaviour(gen_server).

%% API
-export([start_link/1, get_epigpio_proc/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get_epigpio_proc(Client) ->
  gen_server:call(?SERVER, {get_epigpio_proc, Client}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(AppSup) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [AppSup], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #{}} | {ok, State :: #{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([AppSup]) ->
  EpigpioSupSpec = {epigpio_sup,
    {epigpio_sup, start_link, []},
    permanent,
    10000,
    supervisor,
    [epigpio_sup]},
  self() ! {start_epigpio_supervisor, AppSup, EpigpioSupSpec},
  {ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #{}) ->
  {reply, Reply :: term(), NewState :: #{}} |
  {reply, Reply :: term(), NewState :: #{}, timeout() | hibernate} |
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #{}} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_call({get_epigpio_proc, Client}, _From, #{epigpio_sup := Sup} = State) ->
  lager:debug("----> get_epigpio_proc State: ~p, Sup: ~p~n", [State, Sup]),
  {ok, Pid} = supervisor:start_child(Sup, [Client]),
  {reply, {ok, Pid}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_info({start_epigpio_supervisor, AppSup, EpigpioSupSpec}, State) ->
  lager:info("----> Started Epigpio AppSup: ~p,  Spec: ~p~n", [AppSup, EpigpioSupSpec]),
  {ok, Pid} = supervisor:start_child(AppSup, EpigpioSupSpec),
  link(Pid),
  {noreply, State#{epigpio_sup => Pid}};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #{},
    Extra :: term()) ->
  {ok, NewState :: #{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
