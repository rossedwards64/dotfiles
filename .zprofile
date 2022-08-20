#if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}"  -eq 1 ]; then
#      exec startx
#fi

[ "$(tty)" = "/dev/tty1" ] && exec sway
