#!/bin/bash

# curl https://raw.githubusercontent.com/ahri/dotfiles/master/bootstrap.sh | sh

set -ue

ubuntu="apt-transport-https ca-certificates software-properties-common gnupg lsb-release"
misc="htop curl wget lshw man less openssh-client nmap"
build="build-essential cmake git libtinfo-dev zlib1g-dev"
console_dev="neovim dvtm"
snaps="spotify"
snaps_classic="code intellij-idea-community" # stuff that needs access to whole filesystem

if [ -z "$DISPLAY" ]; then
        windowing=""
else
        windowing="xinit x11-xserver-utils unclutter qterminal xclip"
	windowing="$windowing evince chromium-browser firefox"
	windowing="$windowing libglib2.0-bin libgtk2.0-0"
fi

packages="$misc $build $console_dev $windowing"

sudo DEBIAN_FRONTEND=noninteractive apt-get install -qy $packages > /dev/null

if [ ! -z "$DISPLAY" ]; then
	for snap in $snaps; do
		sudo snap install $snap
	done
	for snap in $snaps_classic; do
		sudo snap install --classic $snap
	done
fi

if [ ! -f /usr/share/keyrings/docker-archive-keyring.gpg ]; then
	curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
	echo \
	  "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
	  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
	sudo apt update
	sudo apt install docker-ce docker-ce-cli containerd.io
	sudo gpasswd -a $USER docker
fi

if [ ! -f "$HOME/.ssh/id_ed25519" ]; then
	# https://blog.g3rt.nl/upgrade-your-ssh-keys.html
	ssh-keygen -o -a 100 -t ed25519 -f "$HOME/.ssh/id_ed25519"
	echo
	cat ~/.ssh/id_*.pub
	echo
	read -p "Add key to github then hit [enter]"
fi

if [ ! -d "$HOME/repos/dotfiles" ]; then
	mkdir -p ~/repos
	cd ~/repos
	git clone git@github.com:ahri/dotfiles.git
fi

cd "$HOME/repos/dotfiles"

if [ ! -f /etc/ssh/sshd_config ] || ! grep -q 'AllowGroups remote' /etc/ssh/sshd_config; then
	sudo cp sshd_config /etc/ssh/sshd_config
	sudo groupadd remote
	sudo gpasswd -a $USER remote
fi

for src in .*; do
	if [ "$src" = "." ] || [ "$src" = ".." ] || [ "$src" = ".git" ] || [ "$src" = ".gitignore" ]; then
		continue
	fi

	target="$HOME/$src"
	rm -rf "$target"
	ln -s "$PWD/$src" "$target"
done

cd bin
mkdir -p "$HOME/bin"
for src in *; do
	echo "$src" | grep -q '\.hs$' && continue
	target="$HOME/bin/$src"
	cp "$src" "$target"
done

echo '. "$HOME/.profile_extra"' >> ~/.profile
echo '. "$HOME/.bash_extra"' >> ~/.bashrc

vim -u "$HOME/.vim/installrc.vim" +PlugInstall +qall
