-module(servo_conductor).
-export([start/0, loop/2]).

start() ->
    ServoX = servo:make(x),
    ServoY = servo:make(y),

    loop(ServoX, ServoY).

loop(ServoX, ServoY) ->
    ServoX ! {self(), move, forward},
    timer:sleep(500),
    ServoY ! {self(), move, forward},
    timer:sleep(500),
    ServoX ! {self(), move, back},
    timer:sleep(500),
    ServoY ! {self(), move, back},
    timer:sleep(500),
    loop(ServoX, ServoY).
    