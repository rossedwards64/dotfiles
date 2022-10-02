#!/usr/bin/env bash

default_sink=$(pactl get-default-sink)
headphones_sink="alsa_output.usb-Logitech_G635_Gaming_Headset_00000000-00.analog-stereo"
monitor_sink="alsa_output.pci-0000_13_00.1.hdmi-stereo-extra4"

if [[ "$default_sink" == "$headphones_sink" ]]; then
    pactl set-default-sink $monitor_sink
    notify-send "Changed default sink to monitor"
elif [[ "$default_sink" == "$monitor_sink" ]]; then
    pactl set-default-sink $headphones_sink
    notify-send "Changed default sink to headphones"
fi
