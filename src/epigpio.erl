%%%-------------------------------------------------------------------
%%% @author vasco
%%% @copyright (C) 2016, <Rivux>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2016 10:51
%%%-------------------------------------------------------------------
-module(epigpio).
-author("vasco").

-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-include("epigpio.hrl").
%% API
-export([start_link/1,
         setmode/3,
         getmode/2,
         setpullupdown/3,
         read/2,
         write/3,
         pwm/3,
         get_pwm_dutycycle/2,
         set_pwm_range/3,
         get_pwm_range/2,
         set_pwm_frequency/3,
         get_pwm_frequency/2,
         servo/3,
         get_servo_pulsewidth/2,
         mics/2,
         mils/2,
         tick/1,
         notify_open/1,
         notify_begin/2,
         decode/2, decode/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-type gpio() :: 0..53.
-type level() :: ?PI_LOW | ?PI_HIGH.
-type frequency() :: ?PI_HW_PWM_MIN_FREQ..?PI_HW_PWM_MAX_FREQ.
-type mode() :: 0..7.
-type pud() :: ?PI_PUD_OFF | ?PI_PUD_DOWN | ?PI_PUD_UP.
-type pulsewidth() :: ?PI_SERVO_OFF | ?PI_MIN_SERVO_PULSEWIDTH..?PI_MAX_SERVO_PULSEWIDTH.
-type micros() :: non_neg_integer().
-type millis() :: non_neg_integer().
-type pwm_duty() :: 0..?PI_HW_PWM_RANGE.
-type range() :: ?PI_MIN_DUTYCYCLE_RANGE..?PI_MAX_DUTYCYCLE_RANGE.


-record(state, {cmd_socket, notif_socket, client_pid, notifications=not_started}).

%%%===================================================================
%%% API
%%%===================================================================
%%CommandCode=0 (MODES)
-spec setmode(pid(), gpio(), mode()) -> ok.
setmode(Pid, Gpio, Mode) ->
  gen_server:call(Pid, {setmode, Gpio, Mode}).

%%CC=1 (MODEG)
-spec getmode(pid(), gpio()) -> ok.
getmode(Pid, Gpio) ->
  gen_server:call(Pid, {getmode, Gpio}).

%%CC=2 (PUD)
-spec setpullupdown(pid(), gpio(), pud()) -> ok.
setpullupdown(Pid, Gpio, Pud) ->
  gen_server:call(Pid, {setpullupdown, Gpio, Pud}).

%%CC=3 (READ)
-spec read(pid(), gpio()) -> ok.
read(Pid, Gpio) ->
  gen_server:call(Pid, {read, Gpio}).

%%CC=4 (WRITE)
-spec write(pid(), gpio(), level()) -> ok.
write(Pid, Gpio, Level) ->
  gen_server:call(Pid, {write, Gpio, Level}).

%%CC=5 (PWM)
-spec pwm(pid(), gpio(), pwm_duty()) -> ok.
pwm(Pid, Gpio, Dutycycle) ->
  gen_server:call(Pid, {pwm, Gpio, Dutycycle}).

%%CC=83 (GDC)
-spec get_pwm_dutycycle(pid(), gpio()) -> ok.
get_pwm_dutycycle(Pid, Gpio) ->
  gen_server:call(Pid, {get_pwm_dutycycle, Gpio}).

%%CC=6 (PRS)
-spec set_pwm_range(pid(), gpio(), range()) -> ok.
set_pwm_range(Pid, Gpio, Range) ->
  gen_server:call(Pid, {set_pwm_range, Gpio, Range}).

%%CC=22 (PRG)
-spec get_pwm_range(pid(), gpio()) -> ok.
get_pwm_range(Pid, Gpio) ->
  gen_server:call(Pid, {get_pwm_range, Gpio}).

%%CC=7 (PFS)
-spec set_pwm_frequency(pid(), gpio(), frequency()) -> ok.
set_pwm_frequency(Pid, Gpio, Freq) ->
  gen_server:call(Pid, {set_pwm_frequency, Gpio, Freq}).

%%CC=23 (PFG)
-spec get_pwm_frequency(pid(), gpio()) -> ok.
get_pwm_frequency(Pid, Gpio) ->
  gen_server:call(Pid, {get_pwm_frequency, Gpio}).

%%CC=8 (SERVO)
-spec servo(pid(), gpio(), pulsewidth()) -> ok.
servo(Pid, Gpio, PulseWidth) ->
  gen_server:call(Pid, {servo, Gpio, PulseWidth}).

%%CC=84 (GPW)
-spec get_servo_pulsewidth(pid(), gpio()) -> ok.
get_servo_pulsewidth(Pid, Gpio) ->
  gen_server:call(Pid, {get_servo_pulsewidth, Gpio}).

%%CC=46 (MICS)
-spec mics(pid(), micros()) -> ok.
mics(Pid, Micros) ->
  gen_server:call(Pid, {mics, Micros}).

%%CC=47 (MILS)
-spec mils(pid(), millis()) -> ok.
mils(Pid, Millis) ->
  gen_server:call(Pid, {mils, Millis}).

-spec notify_open(pid()) -> ok.
notify_open(Pid) ->
  gen_server:call(Pid, {notification, notify_open}).

-spec notify_begin(pid(), gpio()) -> ok.
notify_begin(Pid, Gpio) ->
  gen_server:call(Pid, {notification, notify_begin, Gpio}).

%%CC=16 (TICK)
-spec tick(pid()) -> ok.
tick(Pid) ->
  gen_server:call(Pid, {tick}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ClientPid::pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ClientPid) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ClientPid], []).

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
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ClientPid]) ->
  Port = application:get_env(epigpio, pigpiod_port, 8888),
  Ip = application:get_env(epigpio, pigpiod_ip, "127.0.0.1"),
  {ok, CmdSocket} = gen_tcp:connect(Ip, Port, [binary, {packet, 0}]),
  {ok, NotifSocket} = gen_tcp:connect(Ip, Port, [binary, {packet, 0}]),
  {ok, #state{cmd_socket = CmdSocket, notif_socket = NotifSocket, client_pid = ClientPid}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({notification, Cmd}, _From, #state{notif_socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, encode(Cmd, 0, 0, 0)),
  {reply, ok, State};
handle_call({notification, Cmd, P1}, _From, #state{notif_socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, encode(Cmd, P1, 0, 0)),
  {reply, ok, State};
handle_call({Cmd}, _From, #state{cmd_socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, encode(Cmd, 0, 0, 0)),
  {reply, ok, State};
handle_call({Cmd, P1}, _From, #state{cmd_socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, encode(Cmd, P1, 0, 0)),
  {reply, ok, State};
handle_call({Cmd, P1, P2}, _From, #state{cmd_socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, encode(Cmd, P1, P2, 0)),
  {reply, ok, State};
handle_call({Cmd, P1, P2, P3}, _From, #state{cmd_socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, encode(Cmd, P1, P2, P3)),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, notimplemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, Data}, State) when Socket =:= State#state.cmd_socket ->
  lager:debug("Received tcp response: ~p~n",[Data]),
  decode(Data, State#state.client_pid),
  {noreply, State};
%%handle_info({tcp, Socket, <<_Cmd:?UINT, _:?UINT, _:?UINT, _:?UINT, _/binary>> = Data}, State) when byte_size(Data) =:= 16, Socket =:= State#state.notif_socket ->
%%  lager:debug("Received notification CMD response: ~p~n",[Data]),
%%  decode(notification_cmd, Data, State#state.client_pid),
%%  {noreply, State};
handle_info({tcp, Socket, Data}, State) when Socket =:= State#state.notif_socket ->
  lager:debug("Received notification: ~p byte_size: ~p~n",[Data, byte_size(Data)]),
  decode(notification, Data, State#state.client_pid),
%%  Resp = {notification, Seq, Flags, Tick, Level},
%%  send_response(Resp, State#state.client_pid),
  {noreply, State};
handle_info(Info, State) ->
  lager:debug("Received non-expected message: ~p~n",[Info]),
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
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Exta) -r> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_response(Resp, ClientPid) ->
  lager:debug("Sending response ~p to client: ~p~n", [{epigpio, Resp}, ClientPid]),
  ClientPid ! {epigpio, Resp}.

decode(<<>>, _ClientPid) -> ok;
decode(<<?MODES:?UINT, P1:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {setmode, P1, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?MODEG:?UINT, P1:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {getmode, P1, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?PUD:?UINT, Gpio:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {setpullupdown, Gpio, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?READ:?UINT, Gpio:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {read, Gpio, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?WRITE:?UINT, Gpio:?UINT, Level:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {write, Gpio, Level, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?PWM:?UINT, Gpio:?UINT, Duty:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {pwm, Gpio, Duty, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?GDC:?UINT, Gpio:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {get_pwm_dutycycle, Gpio, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?PRS:?UINT, Gpio:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {set_pwm_range, Gpio, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?PRG:?UINT, Gpio:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {get_pwm_range, Gpio, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?PFS:?UINT, Gpio:?UINT, Freq:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {set_pwm_frequency, Gpio, Freq, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?PFG:?UINT, Gpio:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {get_pwm_frequency, Gpio, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?SERVO:?UINT, Gpio:?UINT, PulseWidth:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {servo, Gpio, PulseWidth, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?GPW:?UINT, Gpio:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {get_servo_pulsewidth, Gpio, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?MICS:?UINT, P1:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {mics, P1, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?MILS:?UINT, P1:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {mils, P1, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid);
decode(<<?TICK:?UINT, P1:?UINT, P2:?UINT, Res:?UINT, Rest/binary>>, ClientPid) ->
  Resp = {tick, P1, P2, Res},
  send_response(Resp, ClientPid),
  decode(Rest, ClientPid).


decode(notification, <<>>, _) ->
  ok;
decode(notification, <<?NOIB:?UINT, P1:?UINT, P2:?UINT, P3:?UINT, Rest/binary>> = Data, ClientPid) when byte_size(Data) =:= 16->
  Resp = {notify_open, P1, P2, P3},
  send_response(Resp, ClientPid),
  decode(notification, Rest, ClientPid);
decode(notification, <<?NB:?UINT, P1:?UINT, P2:?UINT, Res:?UINT, Rest/binary>> = Data, ClientPid) when byte_size(Data) =:= 16->
  Resp = {notify_begin, P1, P2, Res},
  send_response(Resp, ClientPid),
  decode(notification, Rest, ClientPid);
decode(notification, <<Seq:16/little, Flags:16/little, Tick:32/little, Level:32/little, Rest/binary>>, ClientPid) ->
  Resp = {notification, Seq, Flags, Tick, Level},
  send_response(Resp, ClientPid),
  decode(notification, Rest, ClientPid).


encode(setmode, Gpio, Mode, _P3) ->
  <<?MODES:?UINT, Gpio:?UINT, Mode:?UINT, 0:?UINT>>;
encode(getmode, Gpio, _P2, _P3) ->
  <<?MODEG:?UINT, Gpio:?UINT, 0:?UINT, 0:?UINT>>;
encode(setpullupdown, Gpio, Pud, _P3) ->
  <<?PUD:?UINT, Gpio:?UINT, Pud:?UINT, 0:?UINT>>;
encode(read, Gpio, _P2, _P3) ->
  <<?READ:?UINT, Gpio:?UINT, 0:?UINT, 0:?UINT>>;
encode(write, Gpio, Level, _P3) ->
  <<?WRITE:?UINT, Gpio:?UINT, Level:?UINT, 0:?UINT>>;
encode(pwm, Gpio, Dutycycle, _P3) ->
  <<?PWM:?UINT, Gpio:?UINT, Dutycycle:?UINT, 0:?UINT>>;
encode(get_pwm_dutycycle, Gpio, _P2, _P3) ->
  <<?GDC:?UINT, Gpio:?UINT, 0:?UINT, 0:?UINT>>;
encode(set_pwm_range, Gpio, Range, _P3) ->
  <<?PRS:?UINT, Gpio:?UINT, Range:?UINT, 0:?UINT>>;
encode(get_pwm_range, Gpio, _P2, _P3) ->
  <<?PRG:?UINT, Gpio:?UINT, 0:?UINT, 0:?UINT>>;
encode(set_pwm_frequency, Gpio, Freq, _P3) ->
  <<?PFS:?UINT, Gpio:?UINT, Freq:?UINT, 0:?UINT>>;
encode(get_pwm_frequency, Gpio, _P2, _P3) ->
  <<?PFG:?UINT, Gpio:?UINT, 0:?UINT, 0:?UINT>>;
encode(servo, Gpio, PulseWidth, _P3) ->
  <<?SERVO:?UINT, Gpio:?UINT, PulseWidth:?UINT, 0:?UINT>>;
encode(get_servo_pulsewidth, Gpio, _P2, _P3) ->
  <<?GPW:?UINT, Gpio:?UINT, 0:?UINT, 0:?UINT>>;
encode(mics, Micros, _P2, _P3) ->
  <<?MICS:?UINT, Micros:?UINT, 0:?UINT, 0:?UINT>>;
encode(mils, Millis, _P2, _P3) ->
  <<?MILS:?UINT, Millis:?UINT, 0:?UINT, 0:?UINT>>;
encode(notify_open, _P1, _P2, _P3) ->
  <<?NOIB:?UINT, 0:?UINT, 0:?UINT, 0:?UINT>>;
encode(notify_begin, Gpio, _P2, _P3) ->
  <<?NB:?UINT, 0:?UINT, Gpio:?UINT, 0:?UINT>>;
encode(tick, 0, 0, 0) ->
  <<?TICK:?UINT, 0:?UINT, 0:?UINT, 0:?UINT>>.
