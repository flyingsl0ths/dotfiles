#!/usr/bin/env bash

##### Programs #####

lxpolkit &

/usr/bin/xautolock -time 10 -locker "$HOME"/.local/bin/i3lock-fancy-rapid -detectsleep &

"$HOME"/.config/polybar/launch.sh "i3"

(nohup picom &)

xinput set-prop 12 316 1

nm-applet &

feh --bg-center "$HOME/Pictures/wallpaper/arch linux miku.png"

gammy &

/usr/lib/xfce4/notifyd/xfce4-notifyd &

###################
