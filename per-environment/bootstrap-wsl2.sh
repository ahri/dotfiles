#!/bin/sh

# curl https://raw.githubusercontent.com/ahri/dotfiles/master/per-environment/bootstrap-wsl2.sh | sh

set -ue
cd

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

if [ ! -f "$HOME/.ssh/id_ed25519" ]; then
	ssh-keygen -o -a 100 -t ed25519 -f "$HOME/.ssh/id_ed25519"
	echo
	cat "$HOME/.ssh/id_ed25519.pub"
	echo
	read -p "Add key to github then hit [enter]"
fi

sudo apt update && sudo apt install -y \
	htop nmap \
	i3 i3status unclutter qterminal xclip feh xdotool x11-xkb-utils keynav \
	build-essential haskell-stack docker.io

curl https://nixos.org/nix/install | sh
. ~/.nix-profile/etc/profile.d/nix.sh
nix-env -iA nixpkgs.neovim nixpkgs.python3Packages.pynvim nixpkgs.ripgrep

mkdir -p ~/Downloads
cd ~/Downloads
wget --continue https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb
sudo apt install --fix-broken -y
cd ..

grep -q profile_extra ~/.profile || echo '. "$HOME/.profile_extra"' >> ~/.profile
grep -q bash_extra ~/.bashrc || echo '. "$HOME/.bash_extra"' >> ~/.bashrc

stack update
stack upgrade
stack install ghcid

mkdir -p ~/repos
cd ~/repos
[ ! -d dotfiles ] && git clone git@github.com:ahri/dotfiles.git
[ ! -d mapdone ] && git clone git@gitlab.com:methiant/mapdone.git
[ ! -d haste-compiler ] && git clone https://github.com/valderman/haste-compiler
(cd dotfiles && ./shake.hs && vim +PluginInstall +qall)
(cd mapdone && ./project.sh test)

echo
echo "Done!"
