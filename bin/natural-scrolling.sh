#!/bin/sh

set -ue

device=`xinput list | grep 'Synaptics' | sed 's,.*id=,,;s,\t.*,,'`
prop=`xinput list-props $device | grep 'Synaptics Scrolling Distance' | sed 's,.*(,,;s,).*,,'`

xinput set-prop $device $prop -106 -106
