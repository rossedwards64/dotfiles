#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

NAME=$(uname -n)
case $NAME in
    ross-desktop)
        monitor_1=$(xrandr -q | cut -d ' ' -f 1 | tail -n +2 | head -n1)
        monitor_2=$(xrandr -q | cut -d ' ' -f 1 | tail -n +20 | head -n1)
        xrandr --output $monitor_1 --primary --mode 1920x1080 --pos 1080x840 --rotate normal --output $monitor_2 --mode 1920x1080 --pos 0x0 --rotate left

        prev_monitor=$(cat /home/ross/.config/i3/config | grep "HDMI" | cut -d ' ' -f 5 | head -n1)
        if [[ $prev_monitor != $monitor_2 ]]; then
            sed -i "s/$prev_monitor/$monitor_2/g" "/home/ross/.config/i3/config"
        fi

        for i in /sys/class/hwmon/hwmon*/temp*_input; do
            dir="$(<$(dirname $i)/name)"
            hwmon="$(readlink -f $i | grep -oP "hwmon[0-9]")"
            conf="/home/ross/.config/polybar/forest/modules.ini"

            if [[ "$dir" == "amdgpu" ]]; then
                sed -i "1005s/hwmon[0-9]/$hwmon/g" $conf
            elif [[ "$dir" == "k10temp" ]]; then
                sed -i "943s/hwmon[0-9]/$hwmon/g" $conf
            fi
        done

        nitrogen --restore &
        picom --experimental-backends -b
        xss-lock --transfer-sleep-lock -- i3lock --nofork --blur=8 --force-clock --bar-indicator --bar-orientation=horizontal \
                                                 --time-pos="960:100" --bar-direction=1 --bar-pos="580" --ind-pos="960:540" --ring-width=10 \
                                                 --time-font=Iosevka:Bold --date-font=Iosevka:Bold --layout-font=Iosevka:Bold --verif-font=Iosevka:Bold \
                                                 --wrong-font=Iosevka:Bold --greeter-font=Iosevka:Bold --verif-text="Verifying password..." \
                                                 --greeter-text="Please enter your password to unlock." \
                                                 --noinput-text="No text has been entered!" --wrong-text="Incorrect password! Try again." --layout-color=ffffffff --time-color=ffffffff \
                                                 --date-color=ffffffff --greeter-color=ffffffff --verif-color=ffffffff --wrong-color=ffffffff --modif-color=ffffffff &
        exec i3 ;;
esac
