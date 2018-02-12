#!/bin/bash

# curl https://raw.githubusercontent.com/ahri/dotfiles/master/bootstrap-new-linux.sh | sh

set -ue

ubuntu="apt-transport-https ca-certificates software-properties-common"
misc="htop curl wget lshw man less openssh-client"
build="build-essential cmake git"
languages="python-pip python3-pip ruby ruby-dev"
docker="docker.io docker-compose"
console_dev="vim dvtm"

if [ $# -eq 0 ]; then
        windowing=""
else
        windowing="xinit x11-xserver-utils i3 i3status unclutter qterminal xclip"
	windowing="$windowing evince chromium-browser firefox"
	windowing="$windowing libglib2.0-bin libgtk2.0-0"
fi

packages="$misc $build $languages $docker $console_dev $windowing"
params=""

pkgmgr="`(which apt-get || which yum) 2> /dev/null || (echo "No supported package manager found" 1>&2 && false)`"

sudo $pkgmgr install -y $params $packages
sudo gem install rake neovim
sudo pip3 install neovim

curl https://nixos.org/nix/install | sh
nix-env -i stack

stack install intero hoogle hlint hspec stylish-haskell

# https://blog.g3rt.nl/upgrade-your-ssh-keys.html
ssh-keygen -o -a 100 -t ed25519 -f "$HOME/.ssh/id_ed25519"
echo
cat ~/.ssh/id_*.pub
echo
read -p "Add key to github then hit [enter]"

mkdir -p ~/repos
cd ~/repos
git config --global user.email "adam@ahri.net"
git config --global user.name "Adam Piper"
git clone git@github.com:ahri/dotfiles.git
cd dotfiles
rake

rake -T
