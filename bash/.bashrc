# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=$PATH:$HOME/.bin:$HOME/.local/bin

# vim mode
set -o vi

#Allows you to cd into directory merely by typing the directory name.
shopt -s autocd

PS1='\[\e[0;95m\]\[\e[0;96m\]\[\e[0;38;5;77m\]\[\e[0m\] \[\e[0;38;5;214m\]λ\[\e[0m\] \[\e[0;38;5;252m\]\w\[\e[0m\] \[\e[0;1;93m\]$(git branch 2>/dev/null | grep '"'"'^*'"'"' | colrm 1 2)\[\e[0m\]'

# env vars
export EDITOR=vim
export PAGER=bat
export READER=zathura
export TERMINAL=kitty
export VISUAL=vim
export SUDO_EDITOR=vim

# aliases
[ -f "$HOME/.bashaliases" ] && source "$HOME/.bashaliases"
[ -f "$HOME/.config/broot/launcher/bash/br" ] && source "$HOME/.config/broot/launcher/bash/br"
