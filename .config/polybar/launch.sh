#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

launch_split_bar() {
  echo "---" | tee -a /tmp/polybar-left.log
  polybar --config=~/.config/polybar/config.ini mainbar-left 2>&1 | tee -a /tmp/polybar-left.log &
  disown

  echo "---" | tee -a /tmp/polybar-center.log
  polybar --config=~/.config/polybar/config.ini mainbar-center 2>&1 | tee -a /tmp/polybar-center.log &
  disown

  echo "---" | tee -a /tmp/polybar-right.log
  polybar --config=~/.config/polybar/config.ini mainbar-right 2>&1 | tee -a /tmp/polybar-right.log &
  disown
}

launch_i3_bar() {
  echo "---" | tee -a /tmp/polybar-i3.log
  polybar --config=~/.config/polybar/config.ini i3-bar 2>&1 | tee -a /tmp/polybar-i3.log &
  disown
}

launch_xmonad_bar() {
  echo "---" | tee -a /tmp/polybar-mainbar.log
  polybar --config=~/.config/polybar/config.ini xmonad-bar 2>&1 | tee -a /tmp/polybar-mainbar.log &
  disown
}

launch_bar() {
  BAR="$1"

  if [ "$BAR" == "split" ]; then
    launch_split_bar
  elif [ "$BAR" == "i3" ]; then
    launch_i3_bar
  elif [ "$BAR" == "xmonad" ]; then
    launch_xmonad_bar
  fi

}

launch_bar "$1"

echo "All bars launched..."
