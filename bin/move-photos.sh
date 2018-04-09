#!/bin/sh

set -ue

if [ ! -d /mnt/sd ] || [ ! -d /mnt/usb ]; then
	echo "dirs /mnt/sd and /mnt/usb must exist" 1>&2
	exit 1
fi

if [ $# -ne 1 ]; then
	echo "provide a title, e.g. india_pics" >&2
	exit 1
fi

cd /mnt/sd/
rsync -av --remove-source-files . /mnt/usb/$1
