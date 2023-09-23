zstyle ':omz:update' mode auto
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd/mm/yyyy"

plugins=(
    archlinux
    colored-man-pages
    command-not-found
    cp
    #emacs
    extract
    fd
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
    ripgrep
    ros
    rsync
    tmux
    torrent
    zsh-autosuggestions
    zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

bindkey -r '^[l'

setopt NO_CASE_GLOB
setopt AUTO_CD
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS

alias zshconfig="emacsclient --create-frame --alternate-editor='' ~/.config/zsh/.zshrc"
alias ohmyzsh="emacsclient --create-frame --alternate-editor='' ~/.local/share/oh-my-zsh"
alias vim="nvim"
alias mv="mv -iv"
alias cp="cp -iv"
alias rm="rm -iv"
alias ls="eza"
alias la="eza -ah"
alias l="eza -lah"
alias cd="z"
alias cat="bat"
alias ..="cd .."
alias grep="rg"
alias find="fd"
alias du="dust -Hr"
alias stow="stow -v"
alias reset-zsh="source ~/.config/zsh/.zshrc"
alias clear="clear && stty sane"
alias cleanup="sudo pacman -Rns $(pacman -Qtdq)"
alias wget="wget --hsts-file=$XDG_DATA_HOME/wget-hsts"

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

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' cache-path $XDG_CACHE_HOME/zsh/zcompcache
zstyle :compinstall filename '/home/ross/.zshrc'

fpath+=$XDG_CONFIG_HOME/.zfunc
autoload -Uz compinit
compinit -d "$XDG_CACHE_HOME"/zsh/zcompdump-"$ZSH_VERSION"

prompt_context() {
    emojis=("⚡" "🔥" "💀" "👑" "😎" "🐸" "🐵" "🌈" "🍻" "🚀" "💡" "🎉" "🔑" "💣" "🚦" "🌙")
    RAND_EMOJI_N=$(( $RANDOM % ${#emojis[@]} + 1 ))
    if [[ "$USER" != "$DEFAULT_USER" || -n "$SSH_CLIENT" ]]; then
        prompt_segment black white "$USER"
    fi
}

eval "$(antidot init)"
eval "$(zoxide init zsh)"

if [[ $TERM == "tramp" ]] && [[ -n $INSIDE_EMACS ]]; then
    unsetopt zle;
    PS1='[\u@\h \W]\$ '
else
    if [ -x "$(command -v starship)" ]; then
        eval "$(starship init zsh)"
    fi
fi

source /home/ross/.config/broot/launcher/bash/br
