#!/usr/bin/env bash

if [[ $1 == "getspeaker" ]]; then
    status=$(amixer sget Master | tail -1 | cut -c 37-38)
    if [[ $status == "of" ]]; then
        notify-send "Muted Speaker"
    elif [[ $status == "on" ]]; then
        notify-send "Unmuted Speaker"
    fi
fi

if [[ $1 == "getmic" ]]; then
    status=$(amixer sget Capture | tail -1 | cut -c 37-38)
    if [[ $status == "of" ]]; then
        notify-send "Muted Microphone"
    elif [[ $status == "on" ]]; then
        notify-send "Unmuted Microphone"
    fi
fi
