#!/usr/bin/env bash

cliphist list |
    rofi -no-config \
        -no-lazy-grab \
        -dmenu \
        -p "Clipboard" \
        -theme ~/.config/rofi/style/clipboard.rasi |
    cliphist decode |
    wl-copy && wtype -M ctrl shift -P v -m ctrl
