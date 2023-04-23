# [ "$(tty)" = "/dev/tty1" ] && exec sway

if [ "$(tty)" = "/dev/tty1" ]; then
    xrandr --output XWAYLAND0 --primary
    exec /home/ross/.local/bin/wrappedHl
fi
