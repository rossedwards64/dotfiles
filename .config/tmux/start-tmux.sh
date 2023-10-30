#!/usr/bin/env sh

while [ -z ${HYPRLAND_INSTANCE_SIGNATURE+x} ]; do
    sleep 1
done

tmuxinator start default
