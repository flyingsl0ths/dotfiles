#!/bin/env bash

kill_startup_apps() {
  BG_APPS=("xautolock" "picom" "nm-applet" "gammy" "xfce4-notifyd")

  for bg_app in ${BG_APPS[*]}; do
    kill "$(pgrep "$bg_app")"
  done

}

##### Startup #####

# Used when restarting the window manager
kill_startup_apps

/usr/bin/xautolock -time 10 -locker ~/.bin/i3lock-fancy-rapid -detectsleep &

"$HOME"/.config/polybar/launch.sh "split"

(nohup picom &)

xinput set-prop 12 316 1

nm-applet &

feh --bg-center "/usr/share/pixmaps/wallpaper/life.png"

gammy &

/usr/lib/xfce4/notifyd/xfce4-notifyd &

###################
