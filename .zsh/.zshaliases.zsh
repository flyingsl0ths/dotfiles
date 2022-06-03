alias lua="lua5.4"

alias py="python3"
alias pip_install="pip3 install --user"
alias bpy="bpython"
alias cc="noglob cc"

alias sql_login="sudo mycli -u root"
alias mariadb_srvc="sudo systemctl start mariadb"
alias st_mariadb_srvc="sudo systemctl stop mariadb"

alias cmake-build="cmake --build"
alias cmake-target="cmake --build . --target"
alias compile-cpp="clang++ -std=c++17 -Wall -Werror -Wextra -Wsign-conversion"
alias compile-c="clang -std=c99 -Wall -Werror -Wextra -Wsign-conversion -pedantic"
alias cmake-c-config="cmake -G Ninja -DCMAKE_C_COMPILER=clang"
alias cmake-cpp-config="cmake -G Ninja -DCMAKE_CXX_COMPILER=clang++"
alias ctv="ctest -VV"

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
#alias tmux="TERM=screen-256color-bce tmux"
#
alias ld="cd -"
alias ls='exa --icons -1'
alias la='exa --icons -1 --all'
alias ll='exa --icons -1 -l'
alias lhi='exa --icons -1 -l -i'

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
alias cat="bat --theme 1337"
alias ctags="ctags --append=yes"
alias ugzp="gzip --uncompress"
alias mtp="jmtpfs -o auto_unmount"
alias paste="xclip -i -sel c"
alias ps="pipes.sh"

alias xmr="xmonad --recompile && xmonad --restart"

alias mic="sudo make install && sudo make clean"

alias zrc="nvim ~/.zshrc"
alias vrc="v ~/.vimrc"
alias vss="v ~/.vimscripts"
alias ncd="nvim  ~/.config/nvim"
alias gtc="nnvim ~/.gitconfig"
alias i3c="nvim ~/.config/i3/config"
alias xmc="nvim ~/.xmonad/xmonad.hs"
alias zsha="nvim ~/.zshaliases"

alias q="exit"
alias of="xdg-open"
alias clr="clear"
alias please="sudo"
alias rm="rm -r"
alias df="df -h"
alias mv="mv -i"
alias kjb="kill -KILL"
alias imv="nsxiv -q"
alias walls="xwallpaper --output eDP"
alias wfo="nmcli radio wifi on"
alias wff="nmcli radio wifi off"
alias krln="uname -rs"
alias h="history"
alias hs="history | rg"
alias hsi="history | rg -s"
alias gp="git pull"
