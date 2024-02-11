#!/usr/bin/env bash

reset-tmux() {
    systemctl --user restart tmux.service
    tmuxinator start default
}

declare required_key
declare required_val

if [[ ${XDG_CURRENT_DESKTOP} = "Hyprland" ]]; then
    echo "Found Hyprland"
    required_key="HYPRLAND_INSTANCE_SIGNATURE"
    required_val=${HYPRLAND_INSTANCE_SIGNATURE}
elif [[ ${XDG_CURRENT_DESKTOP} = "sway" ]]; then
    echo "Found sway"
    required_key="SWAYSOCK"
    required_val=${SWAYSOCK}
fi

while ! tmux capture-pane -p -t1 | grep -E "${required_key}=/.*"; do
    echo "Found ${request-key}, its value is ${required_val}."
    sleep 1
    reset-tmux
done
