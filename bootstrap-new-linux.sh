#!/bin/sh

set -ue

packages="git vim ruby rake rxvt-unicode i3 i3status unclutter dtrx htop"
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
