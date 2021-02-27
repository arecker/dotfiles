setxkbmap -option ctrl:nocaps
xrdb -merge "$HOME/.Xresources"
xscreensaver -no-splash &
$HOME/bin/wallpaper-loop.sh &
exec /bin/i3
