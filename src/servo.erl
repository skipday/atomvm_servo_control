-module(servo).
-export([make/1, init/1]).

% LEDC
-define(LEDC_HIGH_SPEED_MODE, 0).
-define(LEDC_CHANNEL_0, 0).
-define(LEDC_TIMER_13_BIT, 13).
-define(LEDC_TIMER_0, 0).
-define(LEDC_FREQUENCY, 50). % 50Hz for SG90 servo control
-define(LEDC_TIMER, 0).
-define(LEDC_DUTY_RESOLUTION, 13).


make(Servo) -> spawn(?MODULE, init, [Servo]).

init(Servo) ->
    {GPIO, Channel} = case Servo of
        x -> {19, 0};
        y -> {21, 1}
    end,

    ledc:timer_config([
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, ?LEDC_FREQUENCY},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {timer_num, ?LEDC_TIMER_0}
    ]),

    ledc:channel_config([
        {channel, Channel},
        {duty, 0},
        {gpio_num, GPIO},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER_0}
    ]),

    loop(Channel).

loop(Channel) ->
    receive
        {_Pid, move, Direction} ->
            move(Direction, Channel),
            loop(Channel)
    end.

move(forward, Channel) ->
    set_angle(120, Channel);

move(_, Channel) ->
    set_angle(40, Channel).

set_angle(Angle, Channel) when Angle >= 0, Angle =< 180 ->
    Duty = angle_to_duty(Angle),
    ledc:set_duty(?LEDC_HIGH_SPEED_MODE, Channel, Duty),
    ledc:update_duty(?LEDC_HIGH_SPEED_MODE, Channel).

angle_to_duty(Angle) ->
    % SG90 servo: 0 degrees = 0.5ms, 90 degrees = 1.5ms, 180 degrees = 2.5ms
    % Duty cycle for ESP32 LEDC: (pulse width / period) * 2^resolution
    ((Angle * (2500 - 500) div 180) + 500) * (1 bsl ?LEDC_DUTY_RESOLUTION) div (1000000 div ?LEDC_FREQUENCY).