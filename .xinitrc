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

if exists xinput; then
	setxprop()
	{
		device="$1"
		propname="$2"
		shift 2

		prop=`xinput list-props "$device" | grep -v Default | grep "$propname" | sed 's/.*(//;s/).*//'`

		xinput set-prop "$device" "$prop" "$@"
	}

	device=`xinput list | grep 'Synaptics' | sed 's,.*id=,,;s,\t.*,,'`
	if [ -z "$device" ]; then
		echo "Couldn't find synaptics device" 1>&2
	else
		setxprop $device "Natural Scrolling Enabled" 1
		setxprop $device "Tapping Enabled" 1
		setxprop $device "Disable While Typing Enabled" 1
		# TODO: format for below is wrong?
		# setxprop $device "Scroll Method Enabled" 1 0 0
		# setxprop $device "Accel Speed" 1
	fi
elif exists synclient; then
	synclient VertScrollDelta=-150
	synclient HorizScrollDelta=-150
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

#gsettings set org.gnome.desktop.background picture-uri "file://$(dirname $(readlink -f "$0"))/per-environment/laptop-linux/stormboarder.png"
feh --bg-scale "$(dirname $(readlink -f "$0"))/per-environment/laptop-linux/stormboarder.png"

#exec i3
