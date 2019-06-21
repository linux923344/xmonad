#!/bin/bash
echo AUTOSTART
feh --bg-fill $HOME/.wall.jpg &
xset s off -dpms&
xset b off&
xset s off&
xset -dpms&
redshift -l 52.2327:18.3036 -t 6500:3200&
nm-applet --sm-disable&
/usr/local/bin/st -e "tmux-my"&
"/mnt/mega/Systems/Gentoo/apps/KeePass.AppImage"&
megasync&
firefox-bin&
thunderbird-bin&
dunst -config&
/usr/bin/ibus-daemon -d&
sh /home/yorune/.xsessionrc&
transmission-daemon&
teamviewer&
getforecast&
pl&
trayer --edge top --align right --SetDockType true --expand true --SetPartialStrut true --width 6 --transparent true --alpha 0 --widthtype request --tint 0x282c34 --height 23 &
