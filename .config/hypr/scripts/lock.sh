#!/usr/bin/env sh

swayidle -w \
        timeout 300 'swaylock -f -S --effect-blur 10x5' \
        timeout 600 'swaymsg "output * dpms off"' resume 'swaymsg "output * dpms on"' \
        before-sleep 'swaylock -f -S --effect-blur 10x5'
