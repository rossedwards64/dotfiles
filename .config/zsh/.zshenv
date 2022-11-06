typeset -U PATH path

export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_STATE_HOME=${XDG_STATE_HOME:="$HOME/.local/state"}

export LESSHISTFILE=-
export CALCHISTFILE=-

export ZSH="$XDG_DATA_HOME/oh-my-zsh"
export DOOMDIR="$XDG_CONFIG_HOME/doom"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export WINEPREFIX="$XDG_DATA_HOME/wineprefixes/default"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export GOPATH="$XDG_DATA_HOME/go"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME/java"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export ZDOTDIR="$HOME/.config/zsh"
export HISTFILE="$XDG_DATA_HOME/zsh/history"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export LEIN_HOME="$XDG_DATA_HOME/lein"
export GDBHISTFILE="$XDG_DATA_HOME/gdb/history"
export WAKATIME_HOME="$XDG_CONFIG_HOME/wakatime"
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
