#!/bin/sh

set -ue

misc="dtrx htop"
build="rake build-essential cmake git"
languages="python python-dev ruby"
console_dev="vim dvtm abduco"
windowing="i3 i3status unclutter rxvt-unicode"

packages="$misc $languages $console_dev $windowing"
params=""

pkgmgr="`(which apt-get || which yum) 2> /dev/null || (echo "No supported package manager found" 1>&2 && false)`"

case $pkgmgr in
*/apt-get)
    packages="$packages vim-gtk"
    params="-y"
    ;;
*/yum)
    packages="$packages gvim"
esac

$pkgmgr install $params $packages
