#!/bin/sh

set -ue

misc="htop curl wget xclip lshw"
build="build-essential cmake git openjdk-8-jdk"
languages="python python-dev ruby ruby-dev"
console_dev="vim dvtm"
windowing="i3 i3status unclutter qterminal"

packages="$misc $build $languages $console_dev $windowing"
params=""

pkgmgr="`(which apt-get || which yum) 2> /dev/null || (echo "No supported package manager found" 1>&2 && false)`"

$pkgmgr install $params $packages
