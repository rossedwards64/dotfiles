export PATH="$HOME/bin:/usr/local/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/emacs/bin:$HOME/bin:$HOME/.dotfiles/.bin:$XDG_DATA_HOME/cargo/bin:$PATH"
#ZSH_THEME="sorin"
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )
# HYPHEN_INSENSITIVE="true"
zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="dd/mm/yyyy"
# ZSH_CUSTOM=/path/to/new-custom-folder

plugins=(
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
    command-not-found
)

source $ZSH/oh-my-zsh.sh
# export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_GB.UTF-8
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='nvim'
fi

export ALTERNATE_EDITOR=""
export VISUAL="emacsclient -c -a emacs"
export TERM="alacritty"
export HISTCONTROL=ignoreboth
export ARCHFLAGS="-arch x86_64"
export _JAVA_AWT_WM_NONREPARENTING=1
export XCURSOR_SIZE=24
export SDL_VIDEODRIVER=wayland
export MOZ_ENABLE_WAYLAND=1
export GTK_THEME=rose-pine-moon-gtk
export CMAKE_GENERATOR=Ninja

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
alias cd="z"
alias cat="bat"
alias ..="cd .."
alias du="dust -Hr"
alias stow="stow -v"
alias reset-zsh="source ~/.config/zsh/.zshrc"
alias clear="clear && stty sane"
alias cleanup="sudo pacman -Rns $(pacman -Qtdq)"
alias wget=wget --hsts-file="$XDG_DATA_HOME"/wget-hsts

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

if [[ $TERM == "dumb" ]] || [[ $TERM == "tramp" ]]; then
    unsetopt zle;
    PS1='$ '
else
    if [ -x "$(command -v starship)" ]; then
        eval "$(starship init zsh)"
    fi
fi
