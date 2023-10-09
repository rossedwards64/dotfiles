#!/usr/bin/env bash

temp=$(cat /sys/class/drm/card1/device/hwmon/[[:print:]]*/temp1_input | bc)
triple_digits=100000

if [[ "$temp" -ge "$triple_digits" ]]; then
    echo "${temp::-2}°C"
elif [[ "$temp" -lt "$triple_digits" ]]; then
    echo "${temp::-3}°C"
fi
