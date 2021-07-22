#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar-left.log
polybar --config=~/.config/polybar/config.ini  mainbar-left 2>&1 | tee -a /tmp/polybar-left.log & disown

echo "---" | tee -a /tmp/polybar-center.log
polybar --config=~/.config/polybar/config.ini  mainbar-center 2>&1 | tee -a /tmp/polybar-center.log & disown

echo "---" | tee -a /tmp/polybar-right.log
polybar --config=~/.config/polybar/config.ini  mainbar-right 2>&1 | tee -a /tmp/polybar-right.log & disown

echo "All bars launched..."

