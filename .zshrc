export ZSH=$HOME/.oh-my-zsh

export PATH=$PATH:$HOME/.bin:$HOME/.local/bin:$HOME/.gem/ruby/2.7.0/bin:$HOME/.cabal/bin


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

#vars
EDITOR=vim
PAGER=bat
#

# aliases
alias compile-cpp="clang++ -Wall -Werror -std=c++17"
alias compile-c="clang -Wall -Werror -std=c99"
alias ghc="ghc -dynamic"
alias v="vim"

alias ts="tmux new -s"
alias ta="tmux a"
alias tk="tmux kill-session"
alias tls="tmux ls"

alias yt="ytfzf"

alias please="sudo"
alias please_="lxqt-sudo"
alias cat="bat"
alias ls="exa --all --tree --level=1"
alias la="exa -l --all --tree --level=2"
alias mv="mv -i"
alias mkdir="mkdir -p"
alias q="exit"
alias rm="rm -r"
alias df="df -h"
alias sl="ln -s"
alias valgrind="valgrind --leak-check=full"
alias kjb="kill -KILL"
alias zrc="vim ~/.zshrc"
alias vrc="vim ~/.vimrc"
alias code="vscodium"
alias openf="xdg-open"
alias fso="du -h"

alias py="python"

alias cabal_install="cabal install --ghc-options=-dynamic"

alias cmake-c-config="cmake -G Ninja -DCMAKE_C_COMPILER=clang -DCMAKE_EXPORT_COMPILE_COMMANDS=1"
alias cmake-cpp-config="cmake -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=1"
alias cmake-build="cmake --build"
alias cmake-target="cmake --build . --target"

# ryzen laptop related
alias ryzen-cpu-profile="sudo ryzenadj --slow-time=100 --vrmmax-current=300000 --min-gfxclk=4000 --tctl-temp=75 --max-gfxclk=4000 --min-fclk-frequency=1900 --max-fclk-frequency=1900 --stapm-limit=60000 --stapm-time=3600 --fast-limit=60000 --slow-limit=60000 && sudo zenstates -p0 --enable -f 0xea -d 0xd -v 0x53 && sudo zenstates --c6-disable"

alias ryzen-gpu-profile="sudo ryzenadj --slow-time=100 --vrmmax-current=60000 --min-gfxclk=1300 --max-gfxclk=1300 --tctl-temp=75 --min-fclk-frequency=1900 --max-fclk-frequency=1900 --stapm-limit=40000 --stapm-time=3600 --fast-limit=40000 --slow-limit=40000 --min-lclk=5000  --max-lclk=5000 && sudo zenstates -p0 --enable -f 0x68 -d 0xd -v 0x53 && sudo zenstates --c6-disable"

source $ZSH/custom/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
