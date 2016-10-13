#!/bin/sh

set -ue

if [ $# -ne 1 ]; then
	echo "destination please!" >&2
	echo "  e.g. india_pics" >&2
	exit 1
fi

cd sd/
rsync -av --remove-source-files . /mnt/usb/$1
