#!/bin/bash
# Add PATH to ensure we find swaymsg/jq even if environment is weird
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"

source "$HOME/.config/sway/scripts/env.sh"

MODE_FILE="/tmp/sway_wallpaper_mode"

# Default to static if no file exists
if [ ! -f "$MODE_FILE" ]; then
    echo "static" > "$MODE_FILE"
fi

CURRENT_MODE=$(cat "$MODE_FILE")

# --- FUNCTION: STATIC MODE ---
run_static() {
    # Kill video player
    pkill mpvpaper 2>/dev/null

    MONITORS=$(swaymsg -t get_outputs | jq -r '.[] | select(.active) | .name')
    for output in $MONITORS; do
        ORIENTATION=$(get_monitor_orientation "$output")
        TARGET_FOLDER="$WALLPAPER_DIR/$ORIENTATION"
        IMAGE=$(find "$TARGET_FOLDER" -type f | shuf -n 1)

        if [ -n "$IMAGE" ]; then
            swaymsg output "$output" bg "$IMAGE" fill
        fi
    done
}

# --- FUNCTION: VIDEO MODE ---
run_video() {
    # 1. Kill Static Wallpaper (Clean stage)
    pkill swaybg 2>/dev/null

    # 2. Kill old videos
    pkill mpvpaper 2>/dev/null
    sleep 0.2

    MONITORS=$(swaymsg -t get_outputs | jq -r '.[] | select(.active) | .name')
    for output in $MONITORS; do
        ORIENTATION=$(get_monitor_orientation "$output")
        TARGET_FOLDER="$WALLPAPER_DIR/video/$ORIENTATION"
        VIDEO=$(find "$TARGET_FOLDER" -type f \( -name "*.mp4" -o -name "*.mkv" -o -name "*.webm" \) 2>/dev/null | shuf -n 1)

        if [ -n "$VIDEO" ]; then
            # --- THE FIX IS HERE ---
            # We use 'swaymsg exec' to tell SWAY to run the command.
            # We must carefully escape the quotes so the path is handled correctly.
            swaymsg exec "mpvpaper -o 'no-audio --loop-file' \"$output\" \"$VIDEO\""
        fi
    done
}

# --- EXECUTE ---
if [ "$CURRENT_MODE" == "video" ]; then
    run_video
else
    run_static
fi
