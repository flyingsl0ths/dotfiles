export ZSH=$HOME/.oh-my-zsh

export PATH=$PATH:$HOME/.bin:$HOME/.local/bin:$HOME/.local/share/gem/ruby/3.0.0/bin:$HOME/.cabal/bin

ZSH_THEME="norm"

source $ZSH/oh-my-zsh.sh

#
plugins=(zsh-autocomplete zsh-syntax-highlighting git)

# zsh autocomplete setttings
zstyle ':autocomplete:tab:*' widget-style menu-complete
zstyle ':autocomplete:*' min-input 3
#

# enablae vi mode
bindkey -v

#env vars
EDITOR=nvim
PAGER=bat

# aliases
source $HOME/.zshaliases

source $ZSH/custom/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

