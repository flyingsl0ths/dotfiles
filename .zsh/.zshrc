export ZSH=$HOME/.oh-my-zsh

export PATH=$PATH:$HOME/.bin:$HOME/.local/bin:$HOME/.cabal/bin

PROMPT="%F{13}%f%F{14}%f%F{41}%f %F{214}λ%f %F{252}%~%f "

source $ZSH/oh-my-zsh.sh

# enable vi mode
bindkey -v

#
plugins=(zsh-autocomplete zsh-syntax-highlighting git)

# zsh autocomplete setttings
zstyle ':autocomplete:tab:*' widget-style menu-complete
zstyle ':autocomplete:*' min-input 3
#

#env vars
export EDITOR=nvim
export PAGER=bat
export READER=zathura
export TERMINAL=alacritty
export VISUAL=nvim
export SUDO_EDITOR=nvim

# aliases
[ -f ~/.zshaliases ] && source $HOME/.zshaliases

# Save most-recent 1000 lines
SAVEHIST=1000  

source $ZSH/custom/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

[ -f /usr/bin/broot ] && source /home/flyingsloths/.config/broot/launcher/bash/br

# ghcup-env
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" 
