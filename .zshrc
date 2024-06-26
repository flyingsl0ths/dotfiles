export FZF_DEFAULT_COMMAND='fd --type d -H'
export _ZO_ECHO=0
export ZSH_PLUGINS_DIR=$HOME/.zsh
export ANDROID_HOME=$HOME/Android/Sdk
export ANDROID_SDK_ROOT=$HOME/Android/Sdk
export PATH=$PATH:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$HOME/.npm-global:$HOME/.npm-global/bin:$HOME/go/bin:$HOME/.dotnet/tools
export PATH=$PATH:$ANDROID_SDK_ROOT/emulator:$ANDROID_SDK_ROOT/platform-tools
export FZF_DEFAULT_OPTS=" \
--color=bg+:#414559,bg:#303446,spinner:#f2d5cf,hl:#e78284 \
--color=fg:#c6d0f5,header:#e78284,info:#ca9ee6,pointer:#f2d5cf \
--color=marker:#f2d5cf,fg+:#c6d0f5,prompt:#ca9ee6,hl+:#e78284"

[ -f "/home/flyingsl0ths/.ghcup/env" ] && source "/home/flyingsl0ths/.ghcup/env" # ghcup-env

PROMPT="%F{244}%f%F{74}%f%F{111}%f %F{153}𝝺%f %F{7}%~%f "

setopt autocd
setopt NO_BEEP

# eval "$(starship init zsh)"
eval "$(direnv hook zsh)"

HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=999
TERMINAL=/usr/local/bin/alacritty

# enable vi mode
bindkey -v

zstyle ':autocomplete:tab:*' widget-style menu-select
zstyle ':autocomplete:*' min-input 2

pokemon-colorscripts -r --no-title

source $ZSH_PLUGINS_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZSH_PLUGINS_DIR/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source $ZSH_PLUGINS_DIR/zsh-autocomplete/zsh-autocomplete.plugin.zsh

eval "$(zoxide init zsh)"

function cc() eva "($*)"
function sd() cd && cd "$(fzf)"

alias py="python3"
alias pip_install="pip3 install --user"
alias bpy="bpython"
alias cc="noglob cc"

alias sql_login="doas mycli -u root"
alias mariadb_srvc="doas systemctl start mariadb"
alias st_mariadb_srvc="doas systemctl stop mariadb"

alias cmb="cmake --build"
alias cmtg="cmake --target"
alias ccpp="clang++ -std=c++17 -Wall -Werror -Wextra -Wsign-conversion"
alias cmpc="clang -std=c99 -Wall -Werror -Wextra -Wsign-conversion -pedantic"
alias cmcc="cmake -G Ninja -DCMAKE_C_COMPILER=clang"
alias cmcpp="cmake -G Ninja -DCMAKE_CXX_COMPILER=clang++"
alias ctv="ctest -VV"

alias dn="dotnet"

alias nd="node"

alias gdl="gradle"
alias gdw="./gradlew -q"

alias cgo="cargo"

alias cbl="cabal"

alias yt="ytfzf -t -l"
alias ytm="ytfzf -m -t -l"

alias ta="tmux a -t"
alias tls="tmux ls"
alias ts="tmux new -s"
alias tk="tmux kill-session"
#tmux="TERM=screen-256color-bce tmux"

alias ld="cd -"
alias ls="exa --icons -1"
alias la="exa --icons -1 --all"
alias ll="exa --icons -1 -l"
alias lhi="exa --icons -1 -l -i"

alias mkdir="mkdir -p"
alias rmdir="rm -r"

alias pf="paste_file"
alias sp="scratchpad"
alias fm="ranger"
alias xtr="tar xf"
alias code="vscodium"
alias n="nvim"
alias v="TERM=xterm-256color vim"
alias zth="zathura"
alias valgrind="valgrind --leak-check=full"
alias cat="bat --theme Catppuccin-frappe"
alias ctags="ctags --append=yes"
alias ugzp="gzip --uncompress"
alias mtp="jmtpfs -o auto_unmount"
alias paste="xclip -i -sel c"
alias ps="pipes.sh"
alias cb="xclip -i -sel c"
alias zl="zellij"

alias xmr="xmonad --recompile && xmonad --restart"
alias mic="doas make install && doas make clean"

alias vrc="v ~/.vimrc"
alias vss="v ~/.vimscripts"
alias ncd="nvim  ~/.config/nvim"
alias gtc="nnvim ~/.gitconfig"
alias i3c="nvim ~/.config/i3/config"
alias xmc="nvim ~/.xmonad/xmonad.hs"

alias q="exit"
alias of="xdg-open"
alias clr="clear"
alias please="doas"
alias df="df -h"
alias mv="mv -i"
alias kjb="kill -KILL"
alias imv="nsxiv -q"
alias walls="cd ~/.local/share/wallpaper/"
alias wfo="nmcli radio wifi on"
alias wff="nmcli radio wifi off"
alias krln="uname -rs"
alias h="history"
alias hs="history | rg"
alias hsi="history | rg -s"
alias gp="git pull"
alias zrc="nvim ~/.zshrc"
