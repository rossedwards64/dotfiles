#!/usr/bin/bash

# terminate already running bar instances
killall -q polybar
# if all bars have ipc enabled, use
# polybar-msg cmd quit

# launch polybar using default config location ~/.config/polybar/config.ini
echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown

echo "Polybar launched..."
