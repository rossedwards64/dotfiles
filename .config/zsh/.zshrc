plugins=(
    colored-man-pages
    command-not-found
    cp
    extract
    fzf
    git
    gitfast
    gpg-agent
    history
    isodate
    jsontools
    lein
    otp
    pass
    ros
    rsync
    tmux
    torrent
    zsh-autosuggestions
    zsh-syntax-highlighting
)

zstyle ':omz:update' mode auto
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' cache-path $XDG_CACHE_HOME/zsh/zcompcache
zstyle :compinstall filename '/home/ross/.config/.zshrc'
fpath+=$XDG_CONFIG_HOME/.zfunc

ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main brackets cursor root)

source $ZSH/oh-my-zsh.sh

if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]]; then
    export BOLD="$(tput bold)"
    export MAGENTA="$(tput setaf 5)"
    export RED="$(tput setaf 1)"
    export CYAN="$(tput setaf 6)"
    export RMYELLOW="$(tput setaf 3)"
    export GREEN="$(tput setaf 2)"
    export BLUE="$(tput setaf 4)"
    export NORM="$(tput sgr0)"
fi

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd/mm/yyyy"

setopt APPEND_HISTORY
setopt AUTO_CD
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FCNTL_LOCK
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY
setopt NO_CASE_GLOB
setopt SHARE_HISTORY
setopt autocd

alias vim="nvim"
alias mv="mv -iv"
alias cp="cp -iv"
alias rm="rm -iv"
alias ls="eza --icons --color=always"
alias la="eza --icons --color=always -ah"
alias l="eza --icons --color=always -lah"
alias cd="z"
alias cat="bat"
alias ..="cd .."
alias grep="rg"
alias find="fd"
alias du="dust -Hr"
alias stow="stow -v"
alias clear="clear && stty sane"

if [[ ${XDG_CURRENT_DESKTOP} = "sway" ]]; then
    alias zathura="swayhide zathura"
    alias imv="swayhide imv"
    alias mpv="swayhide mpv"
    alias vlc="swayhide vlc"
fi

eval "$(zoxide init zsh)"

if [ "$TERM" != "dumb" -a ! "$INSIDE_EMACS" -a -x "$(command -v starship)" ]; then
    eval "$(starship init zsh)"
fi
