#!/bin/bash

##### Startup #####

/usr/bin/xautolock -time 5 -locker ~/.bin/i3lock-fancy-rapid_sl -detectsleep &

$HOME/.config/polybar/launch.sh

(nohup picom &)

xinput set-prop 12 316 1

nm-applet &

feh --bg-center "/usr/share/pixmaps/wallpaper/tomorrow.png"

gammy &

/usr/lib/xfce4/notifyd/xfce4-notifyd &

###################
