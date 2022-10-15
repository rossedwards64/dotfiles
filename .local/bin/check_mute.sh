#!/usr/bin/env bash

if [[ $1 == "getspeaker" ]]; then
    status=$(pactl get-sink-mute @DEFAULT_SINK@ | cut -c 7-9)
    if [[ $status == "no" ]]; then
        notify-send "Muted Speaker"
    elif [[ $status == "yes" ]]; then
        notify-send "Unmuted Speaker"
    fi
fi

if [[ $1 == "getmic" ]]; then
    status=$(pactl get-source-mute @DEFAULT_SOURCE@ | cut -c 7-9)
    if [[ $status == "no" ]]; then
        notify-send "Muted Microphone"
    elif [[ $status == "yes" ]]; then
        notify-send "Unmuted Microphone"
    fi
fi
