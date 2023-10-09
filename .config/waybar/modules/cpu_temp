#!/usr/bin/env bash

temp=$(cat /sys/devices/pci0000:00/0000:00:18.3/hwmon/[[:print:]]*/temp1_input | bc)
triple_digits=1000000

if [[ "$temp" -ge "$triple_digits" ]]; then
    echo "${temp::-2}°C"
elif [[ "$temp" -lt "$triple_digits" ]]; then
    echo "${temp::-3}°C"
fi
