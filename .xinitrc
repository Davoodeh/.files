#!/bin/sh
# xinitrc runs automatically when you run startx.

# xrandr --setprovideroutputsource modesetting NVIDIA-0 # for Nvidia only systems
xrandr --auto # prevents occasional black screens after `startx` on some machines

# There are some small but important commands that need to be run when we start
# the graphical environment. I keep those commands in ~/.config/xautostart because
# that file is run automatically if someone uses a display manager (login screen)
# and so they are needed there. To prevent doubling up commands, I source them
# here with the line below.
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

case ${1:-${WM:-dwm}} in
    a*) exec awesome ;;
    bs*) exec bspwm ;;
    bu*) exec budgie-desktop ;;
    c*) exec cinnamon-session ;;
    dd*) exec startdde ;;
    dwm) export STATUSBAR="dwmblocks" && ssh-agent dwm ;;
    e*) exec enlightenment_start ;;
    f*) exec startfluxbox ;;
    g*) exec gnome-session ;;
    i3*) export STATUSBAR="i3blocks" && ssh-agent i3 --shmlog-size 0 ;;
    j*) exec jwm ;;
    lxd*) exec startlxde ;;
    lxq*) exec lxqt-session ;;
    m*) exec mate-session ;;
    o*) exec openbox-session ;;
    x*) exec xfce4-session ;;
    k* | plasma-x*) exec startplasma-x11 ;;
    plasma-w*) exec startplasma-wayland ;;
    *) exec ${1:-$WM} ;;
esac
