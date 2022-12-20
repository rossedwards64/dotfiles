#!/usr/bin/env bash

default_sink=$(pactl get-default-sink)
headphones_sink=$(pactl list | grep "Headset" | grep "stereo$" | head -n1 | cut -c 8-77)
monitor_sink=$(pactl list | grep "hdmi" | head -n1 | cut -c 8-47)

if [[ "$default_sink" == "$headphones_sink" ]]; then
    pactl set-default-sink "$monitor_sink"
    notify-send "Changed default sink to monitor"
elif [[ "$default_sink" == "$monitor_sink" ]]; then
    pactl set-default-sink "$headphones_sink"
    notify-send "Changed default sink to headphones"
else notify-send "No valid sinks to switch to."
fi
