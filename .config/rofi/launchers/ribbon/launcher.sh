#!/usr/bin/env bash

dir="$HOME/.config/rofi/launchers/ribbon"

rofi -no-lazy-grab -show drun -modi drun -theme $dir/"$1"
