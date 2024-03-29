#!/usr/bin/env bash

dir="$HOME/.config/rofi/applets/android"
rofi_command="rofi -theme $dir/four.rasi"

# Options
vscode=""
intellij=""
android_studio=""
markdown_editor=""

# Error msg
msg() {
  rofi -theme "$dir/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$vscode\n$intellij\n$android_studio\n$markdown_editor"

chosen="$(echo -e "$options" | $rofi_command -p "Most Used" -dmenu -selected-row 0)"

locate_vscode() {
  if [[ -f "/usr/bin/vscodium" ]]; then
    vscodium &
  elif [[ -f "/usr/bin/code" ]]; then
    code &
  fi
}

case $chosen in
"$vscode") locate_vscode ;;
"$intellij")
  "idea" &
  ;;
"$android_studio")
  "android-studio" &
  ;;
"$markdown_editor")
  "marktext" &
  ;;
esac
