#if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}"  -eq 1 ]; then
#      exec startx
#fi

# [ "$(tty)" = "/dev/tty1" ] && exec sway

[ "$(tty)" = "/dev/tty1" ] && exec /home/ross/.local/bin/wrappedHl
