#!/usr/bin/env bash

cliphist list \
    | rofi -no-config \
           -no-lazy-grab \
           -dmenu \
           -theme ~/.config/rofi/style/launcher.rasi \
    | cliphist decode \
    | wl-copy && wtype -M ctrl shift -P v -m ctrl
