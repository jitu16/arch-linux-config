#!/bin/bash

# 1. Load Global Config
source "$HOME/.config/sway/scripts/env.sh"

# 2. Get Active Monitors
MONITORS=$(swaymsg -t get_outputs | jq -r '.[] | select(.active) | .name')

# 3. Build the Swaylock Command
# We start with the base arguments
LOCK_ARGS=("-f" "--daemonize")

# Loop through each monitor to add specific images
for output in $MONITORS; do
    # A. Get Orientation (Smart + Override)
    ORIENTATION=$(get_monitor_orientation "$output")

    # B. Find a random image in the correct folder
    # Path: ~/Pictures/screensaver/landscape (or portrait)
    IMAGE=$(find "$LOCK_DIR/$ORIENTATION" -type f 2>/dev/null | shuf -n 1)

    # C. Add to arguments list if image exists
    # Adds: -i output:path/to/image.jpg
    if [ -n "$IMAGE" ]; then
        LOCK_ARGS+=("-i" "$output:$IMAGE")
    fi
done

# 4. Execute Swaylock with all built arguments
swaylock "${LOCK_ARGS[@]}"
