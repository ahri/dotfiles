#!/bin/sh

set -ue

packages="git ruby rake rxvt-unicode i3 i3status"

pkgmgr="`(which apt-get || which yum) 2> /dev/null || (echo "No supported package manager found" 1>&2 && false)`"

$pkgmgr install $packages