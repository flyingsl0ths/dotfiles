export ZSH_PLUGINS=$HOME/.zsh-plugins

export PATH=$PATH:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$HOME/.local/share/gem/ruby/3.0.0/bin

eval "$(luarocks path --bin)"

PROMPT="%F{13}%f%F{14}%f%F{41}%f %F{214}  %f%F{252}%~%f "

setopt autocd

# enable vi mode
bindkey -v

zstyle ':autocomplete:tab:*' widget-style menu-select
zstyle ':autocomplete:*' min-input 2

#env vars
export EDITOR=nvim
export PAGER=bat
export READER=zathura
export TERMINAL=st
export VISUAL=nvim
export SUDO_EDITOR=nvim

HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory

# aliases
[ -f ~/.zshaliases ] && source $HOME/.zshaliases

source $ZSH_PLUGINS/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZSH_PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

function cc() python3 -c "from math import *; print($*);"

[ -f /usr/bin/broot ] && source /home/flyingsloths/.config/broot/launcher/bash/br

# ghcup-env
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

source /home/flyingsloths/.config/broot/launcher/bash/br
