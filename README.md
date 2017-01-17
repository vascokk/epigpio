epigpio
=====

An OTP application

Build
-----
Install pigpio and start the pigpiod daemon on the RaspberryPi. See instructions here: http://abyz.co.uk/rpi/pigpio/index.html

Set the pigpiod IP and port in sys.config.

Build the epigpio project:

    $ rebar3 compile
