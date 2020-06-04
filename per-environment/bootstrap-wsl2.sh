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
	build-essential haskell-stack zlib1g-dev

has()
{
	which "$1" > /dev/null 2>&1
}

if ! has rg; then
	curl https://nixos.org/nix/install | sh
	. ~/.nix-profile/etc/profile.d/nix.sh
	nix-env -iA nixpkgs.neovim nixpkgs.python3Packages.pynvim nixpkgs.ripgrep
fi

if ! has google-chrome; then
	mkdir -p ~/Downloads
	cd ~/Downloads
	wget --continue https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
	sudo dpkg -i google-chrome-stable_current_amd64.deb || true
	sudo apt install --fix-broken -y
	cd ..
fi

grep -q profile_extra ~/.profile || echo '. "$HOME/.profile_extra"' >> ~/.profile
grep -q bash_extra ~/.bashrc || echo '. "$HOME/.bash_extra"' >> ~/.bashrc

if ! has ghcid; then
	stack update
	stack upgrade
	stack install ghcid
fi

mkdir -p ~/repos
cd ~/repos
[ ! -d dotfiles ] && git clone git@github.com:ahri/dotfiles.git
[ ! -d mapdone ] && git clone git@gitlab.com:methiant/mapdone.git
[ ! -d haste-compiler ] && git clone https://github.com/valderman/haste-compiler
(cd dotfiles && ./shake.hs && vim +PluginInstall +qall)
(cd mapdone && ./project.sh test)

echo
echo "Done!"
