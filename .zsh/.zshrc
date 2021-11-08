export ZSH=$HOME/.oh-my-zsh

export PATH=$PATH:$HOME/.bin:$HOME/.local/bin:$HOME/.emacs.d/bin

PROMPT="%F{1}[%f%F{51}%n%f%F{11}@%f %F{5}%~%f%F{1}]%f%F{214}λ%f "

source $ZSH/oh-my-zsh.sh

# enablae vi mode
bindkey -v

#
plugins=(zsh-autocomplete zsh-syntax-highlighting git)

# zsh autocomplete setttings
zstyle ':autocomplete:tab:*' widget-style menu-complete
zstyle ':autocomplete:*' min-input 3
#

#env vars
export EDITOR=vim
export PAGER=bat
export READER=zathura
export TERMINAL=kitty
export VISUAL=vim
export SUDO_EDITOR=vim
export WALLPAPERS="$HOME/.local/share/wallhaven"

# aliases
[ -f ~/.zshaliases ] && source $HOME/.zshaliases

SAVEHIST=1000  # Save most-recent 1000 lines

source $ZSH/custom/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

[ -f /usr/bin/broot ] && source /home/flyingsl0ths/.config/broot/launcher/bash/br
