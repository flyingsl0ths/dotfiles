#!/usr/bin/env bash

theme="$HOME/.config/rofi/menus/powermenu.rasi"
rofi_command="rofi -theme $theme"

# Options
shutdown=""
reboot=""
lock=""
suspend=""
logout=""

run_chosen_command() {
  case $chosen in
  "$shutdown")
    systemctl poweroff
    ;;
  "$reboot")
    systemctl reboot
    ;;
  "$lock")
    dm-tool lock
    ;;
  "$suspend")
    systemctl suspend
    ;;
  "$logout")
    kill -9 -1
    ;;
  esac
}

# Variable passed to rofi
options="$suspend\n$logout\n$lock\n$reboot\n$shutdown"

chosen="$(echo -e "$options" | $rofi_command -p "Power Options" -dmenu -selected-row 0)"

run_chosen_command "$chosen"
