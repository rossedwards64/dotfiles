#!/usr/bin/env bash

if [[ $# -eq 0 ]]; then
    grim -g "$(slurp)" - | wl-copy
    exit_status=${PIPESTATUS[0]}
    if [[ exit_status -eq 0 ]]; then
        notify-send "Screenshot taken" "Copied to clipboard"
    else
        notify-send "Screenshot cancelled"
    fi
else
    screenshots_dir="$1"
    mkdir -p "$screenshots_dir"
    grim -g "$(slurp)" - > "$screenshots_dir/$(date +%Y%m%d%H%M%S).png"
    exit_status=$?
    if [[ exit_status -eq 0 ]]; then
        notify-send "Screenshot taken" "Saved to $screenshots_dir"
    else
        notify-send "Screenshot cancelled"
    fi
fi
