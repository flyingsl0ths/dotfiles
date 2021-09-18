#!/bin/env bash

##### Programs #####

/usr/bin/xautolock -time 10 -locker ~/.bin/i3lock-fancy-rapid -detectsleep &

"$HOME"/.config/polybar/launch.sh "i3"

(nohup picom &)

xinput set-prop 12 316 1

nm-applet &

feh --bg-center "/usr/share/pixmaps/wallpaper/matrix.png"

gammy &

/usr/lib/xfce4/notifyd/xfce4-notifyd &

###################
