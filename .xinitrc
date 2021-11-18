# i3 is kept as default
session=${1:-xmonad}

case $session in
    i3|i3wm           ) exec /usr/bin/i3;;
    dwm               ) exec /usr/local/bin/dwm;;
    xmonad            ) exec $HOME/.xmonad/xmonad-x86_64-linux;;
    # No known session, try to run it as command
    *                 ) exec $1;;
esac
