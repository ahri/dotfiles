#!/bin/sh

# curl https://raw.githubusercontent.com/ahri/dotfiles/master/bootstrap-new-linux.sh | sh

set -ue

misc="htop curl wget lshw man less openssh-client"
build="build-essential cmake git"
languages=" python-pip python3-pip ruby ruby-dev"
console_dev="vim dvtm"

if [ $# -eq 0 ]; then
        windowing=""
else
        windowing="i3 i3status unclutter qterminal xclip"
fi

packages="$misc $build $languages $console_dev $windowing"
params=""

pkgmgr="`(which apt-get || which yum) 2> /dev/null || (echo "No supported package manager found" 1>&2 && false)`"

sudo $pkgmgr install -y $params $packages
sudo gem install rake guard neovim
sudo pip3 install --update neovim

# https://blog.g3rt.nl/upgrade-your-ssh-keys.html
ssh-keygen -o -a 100 -t ed25519 -f "$HOME/.ssh/id_ed25519"
echo
cat ~/.ssh/id_*.pub
echo
read -p "Add key to github then hit [enter]"

mkdir -p ~/repos
cd ~/repos
git clone git@github.com:ahri/dotfiles.git
cd dotfiles
rake

rake -T
