#!/bin/sh

cp xmobar.hs xmobar-notray.hs
sed -i -e '/, Run Com "\/home\/bbaovanc\/.xmonad\/xmobar\/trayer-padding-icon.sh" \[\] "trayerpad" 10/d' xmobar-notray.hs
sed -i -e 's/ %trayerpad%/ /' xmobar-notray.hs
