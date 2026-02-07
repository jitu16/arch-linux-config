#!/usr/bin/env sh

# Check if swayidle is running
if pgrep -x "swayidle" > /dev/null
then
    # STOP IT (Coffee Mode)
    pkill swayidle
    notify-send "☕ Coffee Mode ON" "Screensaver Disabled"
else
    # START IT (Using your existing random-lock.sh)
    # I fixed the typo: used 'random-lock.sh' for both lines
    swayidle -w \
        timeout 180 "$HOME/.config/sway/scripts/random-lock.sh" \
        timeout 600 'swaymsg "output * power off"' resume 'swaymsg "output * power on"' \
        before-sleep "$HOME/.config/sway/scripts/random-lock.sh" &

    notify-send "💤 Screensaver ON" "Auto-lock enabled"
fi
