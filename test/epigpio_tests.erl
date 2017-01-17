%%%-------------------------------------------------------------------
%%% @author vasco
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2016 18:49
%%%-------------------------------------------------------------------
-module(epigpio_tests).
-author("vasco").

-compile([debug_info, export_all]).
-compile([{parse_transform, lager_transform}]).

-include_lib("eunit/include/eunit.hrl").
-include("epigpio.hrl").

-define(assertReceived(T),
        receive
          T -> ?assert(true)
        after
          2000 -> ?assert(false)
        end).

commands_test_() ->
  {
    setup,
    local,
    fun () ->
      lager:start(),
      lager:set_loglevel(lager_console_backend, debug),
      application:set_env(epigpio, pigpiod_port, 8888),
      application:set_env(epigpio, pigpiod_ip, "pi3"),
      lager:debug("ClientPid: ~p~n", [self()]),
      epigpio_app:start(["pi3", 8888]),
%%      application:ensure_all_started(epigpio),
      {ok, Pid} = epigpio_app_srv:get_epigpio_proc(self()),
      lager:debug("------>epigpio Pid: ~p~n", [Pid]),
      Pid
    end,
    fun (_) ->
      application:stop(lager),
      application:stop(epigpio)
    end,
    fun(Pid) -> {
      inorder,
      [
        {"Test setmode/getmode",
          fun() -> setgetmode(Pid) end},
        {"Test setpullupdown",
          fun() -> setpullupdown(Pid) end},
        {"Test read/write",
          fun() -> readwrite(Pid) end},
        {"Test pwm",
          fun() -> pwm(Pid) end},
        {"Test notifications",
          fun() -> notify(Pid) end},
        {"Test decode",
          fun() -> decode(Pid) end}
        {"Test tick",
          fun() -> tick(Pid) end}
      ]}
    end

  }.

setgetmode(Pid)->
  lager:debug("ClientPid: ~p~n", [self()]),
  %%?assertEqual(0, element(2,erlang:process_info(self(), message_queue_len))),
  GPIO = 7,
  ?assertEqual(ok, epigpio:setmode(Pid, GPIO, ?PI_OUTPUT)),
  ?assertReceived({epigpio,{setmode, GPIO, ?PI_OUTPUT, 0}}),
  ?assertEqual(ok, epigpio:getmode(Pid, GPIO)),
  ?assertReceived({epigpio,{getmode,GPIO, 0, ?PI_OUTPUT }}).

setpullupdown(Pid)->
  GPIO = 11,
  ?assertEqual(ok, epigpio:setpullupdown(Pid, GPIO, ?PI_PUD_UP)),
  ?assertReceived({epigpio,{setpullupdown, GPIO, ?PI_PUD_UP, 0}}).


readwrite(Pid)->
  GPIO = 7,
  ?assertEqual(ok, epigpio:write(Pid, GPIO, ?PI_HIGH)),
  ?assertReceived({epigpio,{write, GPIO, ?PI_HIGH, 0}}),

  ?assertEqual(ok,epigpio:read(Pid, GPIO)),
  ?assertReceived({epigpio,{read,GPIO, _, ?PI_HIGH}}).


tick(Pid)->

  ?assertEqual(ok, epigpio:tick(Pid)),
  receive
    {epigpio,{tick, 0, 0, Res}} -> lager:debug("Received tick: ~p~n",[Res]),?assert(true)
  after
    2000 -> ?assert(false)
  end.


pwm(Pid)->
  GPIO = 13, Range = 1000, DutyCycle = 500,
  %%Set PWM DC range
  ?assertEqual(ok, epigpio:set_pwm_range(Pid, GPIO, Range)),
  ?assertReceived( {epigpio,{set_pwm_range, GPIO, Range, 250}}),

  %%Get PWM DC range
  ?assertEqual(ok, epigpio:get_pwm_range(Pid, GPIO)),
  ?assertReceived( {epigpio,{get_pwm_range, GPIO, _, Range}}),

  %%Set PWM DutyCycle
  ?assertEqual(ok, epigpio:pwm(Pid, GPIO, DutyCycle)),
  ?assertReceived({epigpio,{pwm, GPIO, DutyCycle, 0}}).


%%Notifications tests
notify(Pid) ->
  GPIO = 27,

  %%  set pin 27 as OUTPUT
  ?assertEqual(ok, epigpio:setmode(Pid, GPIO, ?PI_OUTPUT)),
  ?assertReceived({epigpio,{setmode, GPIO, ?PI_OUTPUT, 0}}),

  %%set pin 27 to HIGH
  ?assertEqual(ok, epigpio:write(Pid, GPIO, ?PI_HIGH)),
  ?assertReceived( {epigpio,{write, GPIO, ?PI_HIGH, 0}}),

  ?assertEqual(ok, epigpio:read(Pid, GPIO)),
  ?assertReceived({epigpio,{read,GPIO, _, ?PI_HIGH}}),

  ?assertEqual(ok, epigpio:notify_open(Pid)),
  ?assertReceived({epigpio,{notify_open, 0, 0, 0}}),

  %start notifications for GPIO#27
  Bits = 1 bsl GPIO,
  ?assertEqual(ok, epigpio:notify_begin(Pid, Bits)),
  ?assertReceived({epigpio,{notify_begin, _, Bits, _}}),

  %%wait for notification for current GPIO level, check the level is PI_HIGH
  receive
    {epigpio,{notification, 0, _, _, Level1}} ->
      ?assertEqual(?PI_HIGH,  (1 bsl GPIO ) band Level1 bsr GPIO)
  after
    2000 -> ?assert(false)
  end,

  %%set pin 27 to LOW
  ?assertEqual(ok, epigpio:write(Pid, GPIO, ?PI_LOW)),
  ?assertReceived({epigpio,{write, GPIO, ?PI_LOW, 0}}),

  %%wait for socket notification for level change, check the level is PI_LOW
  receive
    {epigpio,{notification, 1, _, _, Level2}} ->
      ?assertEqual(?PI_LOW,  (1 bsl GPIO ) band Level2 bsr GPIO)
  after
    2000 -> ?assert(false)
  end.



decode(Pid) ->
  D = <<47,0,0,0,15,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,4,0,0,0,1,0,0,0,0,0,0,0,46,0,0,0,50,0,0,0,0,0,0,0,0,0,0,0>>,
  ?assertEqual(ok, epigpio:decode(D, self())),
  ?assertReceived({epigpio,{mils,15,0,0}}),
  ?assertReceived({epigpio,{write,4,1,0}}),
  ?assertReceived({epigpio,{mics,50,0,0}}).


%%notification_decode_test() ->
%%  D = <<14,0,0,0,216,230,206,215,239,233,0,32,15,0,0,0,20,231,206,215,255,233,0,32,16,0,0,0,45,231,206,215,239,233,0,32,17,0,0,0,105,231,206,215,255,233,0,32,18,0,0,0,180,231,206,215,
%%    239,233,0,32,19,0,0,0,240,231,206,215,255,233,0,32,20,0,0,0,9,232,206,215,239,233,0,32,21,0,0,0,74,232,206,215,255,233,0,32,
%%    22,0,0,0,99,232,206,215,239,233,0,32,23,0,0,0,159,232,206,215,255,233,0,32,24,0,0,0,184,232,206,215,239,233,0,32,25,0,0,0,244,232,206,215,255,201,0,32,26,0,0,0,13,233,206,215,239,
%%    201,0,32,27,0,0,0,68,233,206,215,255,201,0,32,28,0,0,0,148,233,206,215,239,201,0,32,29,0,0,0,203,233,206,215,255,201,0,32,30,0,0,0,233,233,206,215,239,201,0,32,31,0,0,0,32,234,206
%%    ,215,255,201,0,32,32,0,0,0,107,234,206,215,239,201,0,32,33,0,0,0,167,234,206,215,255,201,0,32,34,0,0,0,242,234,206,215,239,201,0,32,35,0,0,0,46,235,206,215,255,201,0,32,36,0,0,
%%    0,71,235,206,215,239,233,0,32,37,0,0,0,136,235,206,215,255,233,0,32,38,0,0,0,161,235,206,215,239,233,0,32,39,0,0,0,221,235,206,215,255,233,0,32,40,0,0,0,246,235,206,215,239,233,
%%    0,32,41,0,0,0,50,236,206,215,255,233,0,32,42,0,0,0,75,236,206,215,239,233,0,32,43,0,0,0,135,236,206,215,255,233,0,32,44,0,0,0,160,236,206,215,239,233,0,32,45,0,0,0,220,236,206,215,255,
%%    233,0,32,46,0,0,0,245,236,206,215,239,233,0,32,47,0,0,0,49,237,206,215,255,233,0,32,48,0,0,0,74,237,206,215,239,233,0,32,49,0,0,0,134,237,206,215,255,233,0,32,50,0,0,0,159,237,20,
%%    215,239,233,0,32,51,0,0,0,219,237,206,215,255,201,0,32,52,0,0,0,244,237,206,215,239,201,0,32,53,0,0,0,53,238,206,215,255,201,0,32,54,0,0,0,128,238,206,215,239,201,0,32,55,0,0,0,
%%    83,238,206,215,255,201,0,32,56,0,0,0,2,239,206,215,239,201,0,32,57,0,0,0,62,239,206,215,255,201,0,32,58,0,0,0,137,239,206,215,239,201,0,32,59,0,0,0,197,239,206,215,255,201,0,32,6,
%%    0,0,0,222,239,206,215,239,201,0,32,61,0,0,0,26,240,206,215,255,233,0,32,62,0,0,0,101,240,206,215,239,233,0,32,63,0,0,0,161,240,206,215,255,233,0,32,64,0,0,0,186,240,206,215,239,
%%    33,0,32,65,0,0,0,246,240,206,215,255,233,0,32,66,0,0,0,65,241,206,215,239,233,0,32,67,0,0,0,125,241,206,215,255,233,0,32,68,0,0,0,150,241,206,215,239,233,0,32,69,0,0,0,210,241,20
%%    ,215,255,233,0,32,70,0,0,0,240,241,206,215,239,233,0,32,71,0,0,0,39,242,206,215,255,233,0,32,72,0,0,0,69,242,206,215,239,233,0,32,73,0,0,0,124,242,206,215,255,233,0,32,74,0,0,0,14,
%%    242,206,215,239,201,0,32,75,0,0,0,209,242,206,215,255,201,0,32,76,0,0,0,239,242,206,215,239,201,0,32,77,0,0,0,38,243,206,215,255,201,0,32,78,0,0,0,68,243,206,215,239,201,0,32,7
%%    ,0,0,0,123,243,206,215,255,201,0,32,80,0,0,0,153,243,206,215,239,201,0,32,81,0,0,0,208,243,206,215,255,201,0,32,82,0,0,0,27,244,206,215,239,201,0,32,83,0,0,0,87,244,206,215,255,21,
%%    0,32,84,0,0,0,112,244,206,215,239,201,0,32,85,0,0,0,177,244,206,215,255,201,0,32,86,0,0,0,202,244,206,215,239,201,0,32,87,0,0,0,6,245,206,215,255,233,0,32,88,0,0,0,31,245,206,
%%    25,239,233,0,32,89,0,0,0,91,245,206,215,255,233,0,32,90,0,0,0,116,245,206,215,239,233,0,32,91,0,0,0,176,245,206,215,255,233,0,32,92,0,0,0,201,245,206,215,239,233,0,32,93,0,0,0,5,
%%    26,206,215,255,233,0,32,94,0,0,0,30,246,206,215,239,233,0,32,95,0,0,0,90,246,206,215,255,233,0,32,96,0,0,0,115,246,206,215,239,233,0,32,97,0,0,0,175,246,206,215,255,233,0,32,98,0,
%%    0,200,246,206,215,239,233,0,32,99,0,0,0,4,247,206,215,255,233,0,32,100,0,0,0,29,247,206,215,239,233,0,32,101,0,0,0,94,247,206,215,255,233,0,32,102,0,0,0,119,247,206,215,239,201,
%%    32,103,0,0,0,179,247,206,215,255,201,0,32,104,0,0,0,204,247,206,215,239,201,0,32,105,0,0,0,8,248,206,215,255,201,0,32,106,0,0,0,33,248,206,215,239,201,0,32,107,0,0,0,93,248,
%%    206,15,255,201,0,32>>,







%%TODO - more tests