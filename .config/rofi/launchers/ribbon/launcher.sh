#!/usr/bin/env bash

theme="full_bottom"

dir="$HOME/.config/rofi/launchers/ribbon"

rofi -no-lazy-grab -show drun -modi drun -theme $dir/"$theme"
