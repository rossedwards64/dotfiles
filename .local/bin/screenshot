#!/usr/bin/env bash

if [[ $# -eq 0 ]]; then
    grim -g "$(slurp)" - | wl-copy && notify-send "Screenshot taken" "Copied to clipboard"
else
    screenshots_dir="$1"
    mkdir -p "$screenshots_dir"
    grim -g "$(slurp)" - > "$screenshots_dir/$(date +%Y%m%d%H%M%S).png" && notify-send "Screenshot taken" "Saved to $screenshots_dir"
fi
