#!/usr/bin/zsh
source "$HOME/.config/hypr/scripts/env.zsh"

# DOCSTRING
# Purpose: Applies a random wallpaper to all active monitors using the awww daemon, selecting from orientation-specific directories.
# Input: None.
# Output: Executes awww image drawing commands or sends an error notification if files are missing.

if ! awww query > /dev/null 2>&1; then
    awww-daemon &
    sleep 0.5
fi

local monitors=($(hyprctl monitors -j | jq -r '.[] | .name'))

for output in $monitors; do
    local orient=$(get_monitor_orientation "$output")
    local files=($WALLPAPER_DIR/$orient/*(.N))

    if (( $#files > 0 )); then
        local random_img=$files[$RANDOM%$#files+1]
        awww img -o "$output" "$random_img" --transition-type grow --transition-fps 60 --transition-step 90
    else
        notify-send "Wallpaper Error" "No images found in $WALLPAPER_DIR/$orient"
    fi
done
