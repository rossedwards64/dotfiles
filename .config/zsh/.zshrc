zstyle ':omz:update' mode auto
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd/mm/yyyy"

plugins=(
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
    command-not-found
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
alias ls="exa"
alias la="exa -ah"
alias l="exa -lah"
alias cat="bat"
alias ..="cd .."
alias du="dust -Hr"
alias stow="stow -v"
alias reset-zsh="source ~/.config/zsh/.zshrc"
alias clear="clear && stty sane"
<<<<<<< HEAD
alias update-world="emerge -uDNa"
alias cleanup="emerge -ac"
alias wget=wget --hsts-file="$XDG_DATA_HOME"/wget-hsts
=======
alias cleanup="sudo emerge --ask --verbose --depclean"
alias upgrade="sudo emerge --ask --verboes --update --deep --newuse --with-bdeps=y --keep-going @world"
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
>>>>>>> 377dc60 (topgrade config)

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

<<<<<<< HEAD
if [[ $TERM == "dumb" ]] || [[ $TERM == "tramp" ]]; then
=======
eval "$(antidot init)"
eval "$(zoxide init zsh)"

if [[ $TERM == "tramp" ]] && [[ -n $INSIDE_EMACS ]]; then
>>>>>>> 377dc60 (topgrade config)
    unsetopt zle;
    PS1='[\u@\h \W]\$ '
else
    if [ -x "$(command -v starship)" ]; then
        eval "$(starship init zsh)"
    fi
fi

source /home/ross/.config/broot/launcher/bash/br
