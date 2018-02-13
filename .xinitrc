[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
setxkbmap gb

exists()
{
	which "$1" > /dev/null
}

ifexists()
{
	exists "$1" || return
	"$@"
}

if exists synclient; then
	synclient VertScrollDelta=-150
	synclient HorizScrollDelta=-150
else
	device=`xinput list | grep 'Synaptics' | sed 's,.*id=,,;s,\t.*,,'`
	# prop=`xinput list-props $device | grep 'Synaptics Scrolling Distance' | sed 's,.*(,,;s,).*,,'`
	# xinput set-prop $device $prop -150 -150

	# alternatively, this uses the "Natural" scroll property
	prop=`xinput list-props $device | grep Natural | grep -v Default | sed 's/.*(//;s/).*//'`
	xinput set-prop $device $prop 1
fi

for p in unclutter dunst notify-osd; do
    killall "$p" 2> /dev/null
done

ifexists dunst &
ifexists unclutter -idle 1 -jitter 2 -root &
ifexists syndaemon -i 1.0 -K -R -t &

if exists gnome-screensaver; then
	gnome-screensaver &
else
	ifexists xscreensaver -no-splash &
fi

xgamma -gamma 0.8

gsettings set org.gnome.desktop.background picture-uri "file://$(dirname $(readlink -f "$0"))/per-environment/laptop-linux/stormboarder.png"

exec i3
