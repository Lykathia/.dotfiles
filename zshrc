# Initialization
autoload -U colors && colors
autoload -U promptinit && promptinit
autoload -U compinit && compinit

# Set Editor
export EDITOR='vim'
bindkey -v

# Annoying SBT autocomplete
unsetopt nomatch

# Keep 10000 lines of history within the shell and save it to ~/.zsh_history:
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

# Keybindings
bindkey "\e[1~" beginning-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\e[5D" backward-word
bindkey "\ee[C" forward-word
bindkey "\ee[D" backward-word

bindkey '^R' history-incremental-search-backward

# Annoying beeps
unsetopt beep
unsetopt hist_beep
unsetopt list_beep

# Terminal Prompt
PROMPT="%{$fg[green]%}%n@%m%#%{$reset_color%} "
RPROMPT="%{$fg_bold[blue]%}%~%{$reset_color%}"

# Aliases
alias pacman='sudo pacman'
alias docker='sudo docker'
alias ls='ls --color=auto'
alias df='df -h -x none'
if [ -f /usr/bin/htop ]; then
    alias top='htop' # Use htop instead of top when available
fi
alias grep='grep -i' # Case insensative

changedir () {
    svn info &> /dev/null
    if [ $? -eq 0 ]; then
        cd `svn info | grep 'Working Copy' | sed 's|Working Copy Root Path: \(.*\)$|\1|g'`
    else
        cd $(git rev-parse --show-toplevel)
    fi
}
alias cdp=changedir
#alias cdp='cd $(git rev-parse --show-toplevel)' # cd to project root

# Grep stuffs
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

# Coloured man pages
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}

# Auto complete rules

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
