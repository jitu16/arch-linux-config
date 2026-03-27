#!/usr/bin/zsh
source "$HOME/.config/hypr/scripts/env.zsh"

# DOCSTRING
# Purpose: Dynamically generates a hyprlock configuration based on connected monitors, injects it into a template, and launches the lock screen.
# Input: None.
# Output: Writes to hyprlock.conf and executes hyprlock.

local template="$HOME/.config/hypr/hyprlock.conf.template"
local config="$HOME/.config/hypr/hyprlock.conf"
local monitor_blocks=""

local monitors=($(hyprctl monitors -j | jq -r '.[] | .name'))

for output in $monitors; do
    local orient=$(get_monitor_orientation "$output")
    local files=($LOCK_DIR/$orient/*(.N))

    if (( $#files > 0 )); then
        local img=$files[$RANDOM%$#files+1]
        monitor_blocks+="background {
    monitor = $output
    path = $img
    blur_passes = 0
    contrast = 0.89
    brightness = 0.8
}
"
    fi
done

if [[ -f "$template" ]]; then
    local content=$(<"$template")
    echo "${content//"# --- MONITOR_CONFIG_START ---"*"# --- MONITOR_CONFIG_END ---"/$monitor_blocks}" > "$config"
else
    echo "Error: Template not found at $template"
    exit 1
fi

hyprlock
