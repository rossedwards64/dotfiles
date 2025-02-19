#!/usr/bin/env bash

for portal in xdg-desktop-portal{,-gtk,-hyprland}.service; do
    if ! systemctl --user is-active --quiet "${portal}"; then
        systemctl --user start "${portal}"
    fi
done
