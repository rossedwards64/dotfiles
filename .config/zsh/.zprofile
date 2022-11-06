export GVIMINIT='let $MYGVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/gvimrc" : "$XDG_CONFIG_HOME/nvim/init.gvim" | so $MYGVIMRC'
export VIMINIT='let $MYVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/vimrc" : "$XDG_CONFIG_HOME/nvim/init.vim" | so $MYVIMRC'

#if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}"  -eq 1 ]; then
#      exec startx "$XDG_CONFIG_HOME/X11/xinitrc"
#fi

[ "$(tty)" = "/dev/tty1" ] && exec sway

# [ "$(tty)" = "/dev/tty1" ] && exec /home/ross/.local/bin/wrappedHl
