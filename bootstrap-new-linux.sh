#!/bin/sh

# curl https://raw.githubusercontent.com/ahri/dotfiles/master/bootstrap-new-linux.sh | sh

set -ue

misc="htop curl wget xclip lshw"
build="build-essential cmake git openjdk-8-jdk"
languages=" python-pip python3-pip ruby ruby-dev"
console_dev="vim dvtm"
windowing="i3 i3status unclutter qterminal"

packages="$misc $build $languages $console_dev $windowing"
params=""

pkgmgr="`(which apt-get || which yum) 2> /dev/null || (echo "No supported package manager found" 1>&2 && false)`"

# https://blog.g3rt.nl/upgrade-your-ssh-keys.html
ssh-keygen -o -a 100 -t ed25519
echo
cat ~/.ssh/id_*.pub
echo
read -p "Add key to github then hit [enter]"

sudo $pkgmgr install $params $packages
sudo gem install rake guard neovim
sudo pip3 install --update neovim

mkdir -p ~/repos
cd ~/repos
git clone git@github.com:ahri/dotfiles.git
cd dotfiles
rake

rake -T
