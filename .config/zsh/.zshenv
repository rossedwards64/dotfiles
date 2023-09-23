# XDG base dirs
export XDG_DATA_HOME="${XDG_DATA_HOME:="$HOME/.local/share"}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:="$HOME/.cache"}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:="$HOME/.config"}"
export XDG_STATE_HOME="${XDG_STATE_HOME:="$HOME/.local/state"}"

# Paths
export PATH="${PATH}:$HOME/.local/bin:$XDG_CONFIG_HOME/emacs/bin:"`
            `"$HOME/bin:$HOME/.dotfiles/.bin:$XDG_DATA_HOME/cargo/bin"
export MANPATH="/usr/local/man:$MANPATH"

# ZSH dirs
export ZSH="$XDG_DATA_HOME/oh-my-zsh"
export ZDOTDIR="$HOME/.config/zsh"
export ZSH_COMPDUMP="$ZSH/cache/.zcompdump-$HOST"

# Shell enviornment
export LANG="en_GB.UTF-8"
export EDITOR='emacsclient -c -a ""'
export ALTERNATE_EDITOR="nvim"
export BROWSER="firefox"
export VISUAL='emacsclient -c -a ""'
export TERM="alacritty"
export ARCHFLAGS="-arch x86_64"
export GTK_THEME="rose-pine-moon-gtk"
export XCURSOR_SIZE=24
#export SDL_VIDEODRIVER="wayland"
export MOZ_ENABLE_WAYLAND=1
export CMAKE_GENERATOR="Ninja"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"

# Home dirs
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export EMACSDIR="$XDG_CONFIG_HOME/emacs"
export DOOMDIR="$XDG_CONFIG_HOME/doom"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$XDG_DATA_HOME/go"
export LEIN_HOME="$XDG_DATA_HOME/lein"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export WAKATIME_HOME="$XDG_CONFIG_HOME/wakatime"
export WINEPREFIX="$XDG_DATA_HOME/wineprefixes/default"
export KDEHOME="$XDG_CONFIG_HOME/kde"
export ROSWELL_HOME="$XDG_CONFIG_HOME/roswell"

# Startup files
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"

# History files
export HISTFILE="$XDG_DATA_HOME/zsh/history"
export HISTCONTROL="ignoreboth"
export GDBHISTFILE="$XDG_DATA_HOME/gdb/history"
export LESSHISTFILE=-
export CALCHISTFILE=-

# Java environment
export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME="/usr/lib/jvm/default/"
export _JAVA_OPTIONS='-Djava.util.prefs.userRoot='"$XDG_CONFIG_HOME"'/java, '`
                     `'-Dawt.useSystemAAFontSettings=on, -Dswing.aatext=true'
export JDK_JAVA_OPTIONS="$_JAVA_OPTIONS"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"

# Workarounds
export PROTON_USE_WINED3D=1
export MESA_GL_VERSION_OVERRIDE=4.6
export MESA_GLSL_VERSION_OVERRIDE=460
