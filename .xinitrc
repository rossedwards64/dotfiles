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
        exec startkde ;;
    ross-laptop)
        setxkbmap gb
        exec startkde ;;
    ross-thinkpad)
        nitrogen --restore &
        picom --experimental-backends -b
        xss-lock --transfer-sleep-lock -- i3lock --nofork --blur=8 --force-clock --bar-indicator --bar-orientation=horizontal \
                                                 --time-pos="683:100" --bar-direction=1 --bar-pos="424" --ind-pos="683:384" --ring-width=10 \
                                                 --time-font=Iosevka:Bold --date-font=Iosevka:Bold --layout-font=Iosevka:Bold --verif-font=Iosevka:Bold \
                                                 --wrong-font=Iosevka:Bold --greeter-font=Iosevka:Bold --verif-text="Verifying password..." \
                                                 --greeter-text="Please enter your password to unlock." \
                                                 --noinput-text="No text has been entered!" --wrong-text="Incorrect password! Try again." --layout-color=ffffffff --time-color=ffffffff \
                                                 --date-color=ffffffff --greeter-color=ffffffff --verif-color=ffffffff --wrong-color=ffffffff --modif-color=ffffffff &
        setxkbmap gb
        exec i3 ;;
esac
