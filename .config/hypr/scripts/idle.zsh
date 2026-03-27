#!/usr/bin/zsh

# DOCSTRING
# Purpose: Toggles the hypridle daemon process and sends a system notification regarding its state.
# Input: None.
# Output: Terminates or starts hypridle and triggers notify-send.

if pgrep -x "hypridle" > /dev/null; then
    pkill hypridle
    notify-send "Coffee Mode ON" "Idle behavior disabled"
else
    hypridle &
    notify-send "Screensaver ON" "Hypridle activated"
fi
