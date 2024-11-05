export FZF_DEFAULT_COMMAND='fd --type d -H'
export ZSH_PLUGINS_DIR=$HOME/.zsh
export _ZO_ECHO=0
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
    --color=fg:#e5e9f0,bg:#3b4252,hl:#81a1c1
    --color=fg+:#e5e9f0,bg+:#3b4252,hl+:#81a1c1
    --color=info:#eacb8a,prompt:#bf6069,pointer:#b48dac
    --color=marker:#a3be8b,spinner:#b48dac,header:#a3be8b'

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  export ANDROID_HOME=$HOME/Android/Sdk
  export ANDROID_SDK_ROOT=$ANDROID_HOME
  export PATH=$PATH:$ANDROID_SDK_ROOT/emulator:$ANDROID_SDK_ROOT/platform-tools
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
  export ANDROID_HOME=$HOME/Library/Android/sdk
  export ANDROID_SDK_ROOT=$ANDROID_HOME
  export PATH=$PATH:$ANDROID_SDK_ROOT/emulator:$ANDROID_SDK_ROOT/platform-tools
  export PATH="/opt/homebrew/opt/llvm/bin:$PATH:/opt/homebrew/bin"
  for version in $(ls "$HOME/Library/Python"); do
      export PATH=$PATH:"$HOME/Library/Python/$version/bin"
  done
fi

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null
# END opam configuration

export PATH=$PATH:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$HOME/.npm-global:$HOME/.npm-global/bin:$HOME/go/bin

PROMPT="%F{244}%f%F{74}%f%F{111}%f %F{153}𝝺%f %F{7}%~%f "

setopt autocd
setopt NO_BEEP

eval "$(direnv hook zsh)"

HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=999

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  TERMINAL=/usr/local/bin/alacritty
elif [[ "$OSTYPE" == "darwin"* ]]; then
  TERMINAL=/Applications/Alacritty.app/Contents/MacOS/alacritty
fi

# enable vi mode
bindkey -v

zstyle ':autocomplete:tab:*' widget-style menu-select
zstyle ':autocomplete:*' min-input 2

#pokemon-colorscripts -r --no-title

source $ZSH_PLUGINS_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZSH_PLUGINS_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $ZSH_PLUGINS_DIR/zsh-autocomplete/zsh-autocomplete.plugin.zsh

eval "$(zoxide init zsh)"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f $HOME/.config/broot/launcher/bash/br ] && source $HOME/.config/broot/launcher/bash/br

function sd() cd && cd $(fzf)

alias py="python3.12"
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
alias gdxst="java -jar ~/.local/bin/gdx-setup.jar"

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
alias tk="tmux kill-session -t"
#tmux="TERM=screen-256color-bce tmux";

alias ld="cd -"
alias ls="eza --icons -1"
alias la="eza --icons -1 --all"
alias ll="eza --icons -1 -l"
alias lhi="eza --icons -1 -l -i"
alias dcr="gpg --quiet -d"

alias mkdir="mkdir -p"
alias rmdir="rm -r"

alias pf="paste_file"
alias sp="scratchpad"
alias fm="ranger"
alias xtr="tar xf"
alias code="vscodium"
alias zed="zeditor"
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

alias xmr="xmonad --recompile && xmonad --restart"
alias mic="doas make install && doas make clean"

alias vrc="v ~/.vimrc"
alias vss="v ~/.vimscripts"
alias nvc="nvim  ~/.config/nvim"
alias gtc="nnvim ~/.gitconfig"
alias i3c="nvim ~/.config/i3/config"
alias xmc="nvim ~/.xmonad/xmonad.hs"

alias q="exit"
alias of="xdg-open"
alias clr="clear"
alias please="doas"
alias df="df -h"
alias mv="mv -i"
alias kk="kill -KILL"
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
alias cd="z"
alias purs-f="purs-tidy format-in-place"
alias lzg="lazygit"
