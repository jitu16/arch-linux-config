#!/usr/bin/bash

# --- 1. GLOBAL PATHS ---
# Central location for your media
export WALLPAPER_DIR="$HOME/Pictures/wallpapers"
export LOCK_DIR="$HOME/Pictures/screensaver"

# --- 2. ORIENTATION LOGIC ---
# Function: Returns "portrait" or "landscape" for a given monitor name
get_monitor_orientation() {
    local output_name=$1

    # --- A. MANUAL OVERRIDES ---
    # EDIT THIS to force your specific setup!
    case "$output_name" in
        "HDMI-A-1")
            echo "portrait"
            return
            ;;
        # Add more overrides here if needed:
        # "DP-2") echo "landscape" ;;
    esac

    # --- B. AUTO-DETECTION (Fallback) ---
    # If no override exists, ask Sway: "Is Height > Width?"
    # Requires 'jq' (which you installed earlier)
    local orientation=$(swaymsg -t get_outputs | jq -r --arg name "$output_name" \
        '.[] | select(.name == $name) | if .rect.height > .rect.width then "portrait" else "landscape" end')

    # Safety Net: If detection fails, default to landscape
    if [ -z "$orientation" ] || [ "$orientation" == "null" ]; then
        echo "landscape"
    else
        echo "$orientation"
    fi
}
