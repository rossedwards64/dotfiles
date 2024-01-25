#!/usr/bin/env bash

reset-tmux() {
    systemctl --user restart tmux.service
    tmuxinator start default
}

declare required_val

if [[ ${XDG_CURRENT_DESKTOP} = "Hyprland" ]]; then
    echo "Found Hyprland"
    required_val=${HYPRLAND_INSTANCE_SIGNATURE}
elif [[ ${XDG_CURRENT_DESKTOP} = "sway" ]]; then
    echo "Found sway"
    required_val=${SWAYSOCK}
fi

if [[ -z ${required_val+x} ]]; then
    sleep 1
    reset-tmux
else
    tmuxinator start default
fi
