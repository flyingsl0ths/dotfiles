def create_left_prompt [] {
    let path_segment = if (is-admin) {
        $"(ansi red_bold)($env.PWD)"
    } else {
        $"(ansi cyan_bold)($env.PWD)"
    }

    $path_segment
}

def create_right_prompt [] {
    ""
}

# Kills all processes who's name matches the given pattern
def kall [
  pattern: string # The pattern describing all matching processes
] {
    pgrep $pattern | lines | each {|it| kill ($it | into int)}
    null
}

# Use nushell functions to define your right and left prompt
let-env PROMPT_COMMAND = { create_left_prompt }
let-env PROMPT_COMMAND_RIGHT = { create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
let-env PROMPT_INDICATOR = { "  " }
let-env PROMPT_INDICATOR_VI_INSERT = { "   " }
let-env PROMPT_INDICATOR_VI_NORMAL = { "  " }
let-env PROMPT_MULTILINE_INDICATOR = { "-- " }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
let-env ENV_CONVERSIONS = {
  "PATH": {
    from_string: { |s| $s | split row (char esep) | path expand -n }
    to_string: { |v| $v | path expand -n | str join (char esep) }
  }
  "Path": {
    from_string: { |s| $s | split row (char esep) | path expand -n }
    to_string: { |v| $v | path expand -n | str join (char esep) }
  }
}

# Directories to search for scripts when calling source or use
#
# By default, <nushell-config-dir>/scripts is added
let-env NU_LIB_DIRS = [
    ($nu.config-path | path dirname | path join 'scripts')
]

# Directories to search for plugin binaries when calling register
#
# By default, <nushell-config-dir>/plugins is added
let-env NU_PLUGIN_DIRS = [
    ($nu.config-path | path dirname | path join 'plugins')
]

let-env ANDROID_HOME = $"($env.HOME)/Android/Sdk"
let-env ANDROID_SDK_ROOT = $"($env.HOME)/Android/Sdk"

let-env PATH = (
  $env.PATH
  | append $"($env.HOME)/.local/bin"
  | append $"($env.HOME)/.cabal/bin"
  | append $"($env.HOME)/.cargo/bin" 
  | append $"($env.HOME)/.ghcup/bin"
  | append $"($env.HOME)/.npm-global"
  | append $"($env.HOME)/.npm-global/bin"
  | append $"($env.HOME)/go/bin"
  | append $"($env.ANDROID_SDK_ROOT)/emulator"
  | append $"($env.ANDROID_SDK_ROOT)/platform-tools"
  ) 

alias py = python3
alias pip_install = pip3 install --user
alias bpy = bpython
alias cc = eva

alias sql_login = doas mycli -u root
alias mariadb_srvc = doas systemctl start mariadb
alias st_mariadb_srvc = doas systemctl stop mariadb

alias cmb = cmake --build
alias cmtg = cmake --target
alias ccpp = clang++ -std = c++17 -Wall -Werror -Wextra -Wsign-conversion
alias cmc = clang -std = c99 -Wall -Werror -Wextra -Wsign-conversion -pedantic
alias cmcc = cmake -G Ninja -DCMAKE_C_COMPILER = clang
alias cmcpp = cmake -G Ninja -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_CXX_INCLUDE_WHAT_YOU_USE=include-what-you-use
alias ctv = ctest -VV

alias nd = node

alias gdl = gradle
alias gdw = ./gradlew -q

alias cgo = cargo

alias cbl = cabal

alias yt = ytfzf -t -l
alias ytm = ytfzf -m -t -l

alias ta = tmux a -t
alias tls = tmux ls
alias ts = tmux new -s
alias tk = tmux kill-session
#tmux = TERM = screen-256color-bce tmux

alias ld = cd -
alias la = exa --icons -1 --all
alias l1 = exa --icons -1 -l
alias lh1 = exa --icons -1 -l -i
alias rmd = rm -r

alias pf = paste_file
alias sp = scratchpad
alias fm = ranger
alias xtr = tar xf
alias code = vscodium
alias nv = nvim
alias hx = helix
alias vm = TERM = xterm-256color vim
alias zth = zathura
alias valgrind = valgrind --leak-check = full
alias cat = bat --theme Catppuccin-frappe
alias ctags = ctags --append = yes
alias ugzp = gzip --uncompress
alias mtp = jmtpfs -o auto_unmount
alias paste = xclip -i -sel c
alias pps = pipes.sh
alias cb = xclip -i -sel c
alias br = broot

alias xmr = xmonad --recompile && xmonad --restart
alias mic = doas make install && doas make clean

alias vrc = v ~/.vimrc
alias vss = v ~/.vimscripts
alias ncd = nvim  ~/.config/nvim
alias gtc = nnvim ~/.gitconfig
alias i3c = nvim ~/.config/i3/config
alias xmc = nvim ~/.xmonad/xmonad.hs

alias q = exit
alias of = xdg-open
alias clr = clear
alias please = doas
alias df = df -h
alias mv = mv -i
alias kjb = kill -KILL
alias imv = nsxiv -q
alias walls = cd ~/.local/share/wallpaper/
alias wfo = nmcli radio wifi on
alias wff = nmcli radio wifi off
alias krln = uname -rs
alias hy = history
alias gp = git pull
alias zrc = nvim ~/.zshrc
