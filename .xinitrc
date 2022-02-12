session=${1:-dwm}

case $session in
    i3|i3wm           ) exec /usr/bin/i3;;
    dwm               ) exec /usr/local/bin/dwm;;
    xmonad            ) exec $HOME/.xmonad/xmonad-x86_64-linux;;
    awesome           ) exec /usr/bin/awesome;;
    # No known session, try to run it as command
    *                 ) exec $1;;
esac
