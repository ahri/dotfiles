#!/bin/sh

set -ue

misc="dtrx htop curl wget silversearcher-ag xclip"
build="rake build-essential cmake git openjdk-8-jdk"
languages="python python-dev ruby"
console_dev="vim dvtm"
windowing="i3 i3status unclutter rxvt-unicode-256color"

packages="$misc $build $languages $console_dev $windowing"
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
