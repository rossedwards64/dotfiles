#!/usr/bin/env bash

gpu_dir=/sys/class/drm/card1/device
gpu_percent=$(cat $gpu_dir/gpu_busy_percent)

echo "$gpu_percent%"
