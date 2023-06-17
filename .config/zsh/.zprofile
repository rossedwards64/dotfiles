<<<<<<< HEAD
export GVIMINIT='let $MYGVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/gvimrc" : "$XDG_CONFIG_HOME/nvim/init.gvim" | so $MYGVIMRC'
export VIMINIT='let $MYVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/vimrc" : "$XDG_CONFIG_HOME/nvim/init.vim" | so $MYVIMRC'

#[[ -f ~/.config/zsh/.zshrc ]] && source ~/.config/zsh/.zshrc
#[[ -t 0 && $(tty) == /dev/tty1 && ! $DISPLAY ]] && exec startx "$XDG_CONFIG_HOME/X11/xinitrc"

[ "$(tty)" = "/dev/tty1" ] && exec sway

# [ "$(tty)" = "/dev/tty1" ] && exec /home/ross/.local/bin/wrappedHl
=======
if [ "$(tty)" = "/dev/tty1" ]; then 
    #dbus-launch startplasma-x11
    dbus-launch --exit-with-session startplasma-wayland
fi
>>>>>>> 377dc60 (topgrade config)
