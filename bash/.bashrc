# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source "/usr/share/bash-completion/bash_completion"

export PATH=$PATH:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$HOME/.local/share/gem/ruby/3.0.0/bin

# vim mode
set -o vi

#Allows you to cd into directory merely by typing the directory name.
shopt -s autocd

PS1="\[\e[0;95m\]\[\e[0;96m\]\[\e[0;38;5;77m\]\[\e[0m\] \[\e[0;38;5;214m\]λ\[\e[0m\] \[\e[0;38;5;252m\]\w\[\e[0m\] \[\e[0;1;93m\]$(git branch 2>/dev/null | grep '"'"'^*'"'"' | colrm 1 2)\[\e[0m\]"

# env vars
export EDITOR=nvim
export PAGER=bat
export READER=zathura
export TERMINAL=st
export VISUAL=nvim
export SUDO_EDITOR=nvim

function cc() {
    python3 -c "from math import *; print($@);"
}

# aliases
[ -f "$HOME/.bashaliases" ] && source "$HOME/.bashaliases"
[ -f /usr/bin/broot ] && source /home/flyingsloths/.config/broot/launcher/bash/br

# ghcup-env
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

source "${HOME}/.config/broot/launcher/bash/br"
