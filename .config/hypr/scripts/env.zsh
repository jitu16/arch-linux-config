#!/usr/bin/zsh

export WALLPAPER_DIR="$HOME/Pictures/wallpapers"
export LOCK_DIR="$HOME/Pictures/screensaver"

# DOCSTRING
# Purpose: Determines the orientation of a given monitor by comparing its width and height, accounting for hardware rotation.
# Input: $1 (string) - The exact name of the monitor (e.g., "HDMI-A-1").
# Output: string - Returns "landscape" or "portrait".
get_monitor_orientation() {
    local monitor_name=$1
    local monitor_info=$(hyprctl monitors -j | jq -r --arg name "$monitor_name" '.[] | select(.name == $name)')

    local transform=$(echo "$monitor_info" | jq -r '.transform')
    local width=$(echo "$monitor_info" | jq -r '.width')
    local height=$(echo "$monitor_info" | jq -r '.height')

    if [[ $((transform % 2)) -eq 1 ]]; then
        if (( width > height )); then echo "portrait"; else echo "landscape"; fi
    else
        if (( width > height )); then echo "landscape"; else echo "portrait"; fi
    fi
}
