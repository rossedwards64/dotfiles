#!/bin/bash

tmux splitw -h -p 50 "gdbserver :4444 $1"
tmux selectp -t 0
gdb -x ~/.config/gdb/scripts/debug.gdb
