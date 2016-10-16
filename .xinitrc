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
	prop=`xinput list-props $device | grep 'Synaptics Scrolling Distance' | sed 's,.*(,,;s,).*,,'`
	xinput set-prop $device $prop -150 -150
fi

for p in unclutter dunst notify-osd; do
    killall "$p" 2> /dev/null
done

ifexists dunst &
ifexists unclutter -idle 1 -jitter 2 -root &

mouse-disable

xscreensaver &
wicd-client -t &

exec i3
