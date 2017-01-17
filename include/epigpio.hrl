%%%-------------------------------------------------------------------
%%% @author vasco
%%% @copyright (C) 2016, <Rivux>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2016 15:02
%%%-------------------------------------------------------------------
-author("vasco").

-define(UINT,32/little).

%%GPIO modes
-define(PI_INPUT, 0).
-define(PI_OUTPUT, 1).
-define(PI_ALT0, 4).
-define(PI_ALT1, 5).
-define(PI_ALT2, 6).
-define(PI_ALT3, 7).
-define(PI_ALT4, 3).
-define(PI_ALT5, 2).

%%GPIO Levels
-define(PI_OFF, 0).
-define(PI_ON, 1).

-define(PI_CLEAR, 0).
-define(PI_SET, 1).

-define(PI_LOW, 0).
-define(PI_HIGH, 1).

-define(PI_PUD_OFF, 0).
-define(PI_PUD_DOWN, 1).
-define(PI_PUD_UP, 2).

-define(PI_HW_PWM_MIN_FREQ, 1).
-define(PI_HW_PWM_MAX_FREQ, 125000000).

-define(PI_SERVO_OFF, 0).
-define(PI_MIN_SERVO_PULSEWIDTH, 500).
-define(PI_MAX_SERVO_PULSEWIDTH, 2500).

-define(PI_HW_PWM_RANGE, 1000000).
-define(PI_MIN_DUTYCYCLE_RANGE, 25).
-define(PI_MAX_DUTYCYCLE_RANGE, 40000).

%%Commands
-define(MODES, 0).
-define(MODEG, 1).
-define(PUD,   2).
-define(READ,  3).
-define(WRITE, 4).
-define(PWM,   5).
-define(PRS,   6).
-define(PFS,   7).
-define(SERVO, 8).
-define(TICK, 16).
-define(NB,   19).
-define(PRG,  22).
-define(PFG,  23).
-define(MICS, 46).
-define(MILS, 47).
-define(GDC,  83).
-define(GPW,  84).
-define(NOIB, 99).
