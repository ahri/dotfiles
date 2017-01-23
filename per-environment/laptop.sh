#!/bin/bash

set -ue

gsettings set org.gnome.desktop.background picture-uri "file://$(dirname $(readlink -f "$0"))/wallpaper_1280x800.png"

cat <<AS_ROOT | sudo sh
set -ue

# LED flashing is annoying
([ ! -e /etc/modprobe.d/iwlwifi.conf ] || grep -qv led_mode < /etc/modprobe.d/iwlwifi.conf) && echo "options iwlwifi led_mode=1" >> /etc/modprobe.d/iwlwifi.conf

# Apparently yields battery-life benefits, or maybe freezes...
grep -q pcie_aspm < /etc/default/grub || (awk '/^GRUB_CMDLINE_LINUX/ { sub(/"$/, " pcie_aspm=force\""); } { print; }' < /etc/default/grub > /tmp/__grub && mv /tmp/__grub /etc/default/grub && (grub-mkconfig || grub2-mkconfig))

# Nicer font rendering
cd /etc/fonts/conf.d
ln -sf /usr/share/fontconfig/conf.avail/10-autohint.conf
ln -sf /usr/share/fontconfig/conf.avail/11-lcdfilter-default.conf

# note: "syndaemon -i 1.0 -K -R -t" for laptop

# https://wiki.archlinux.org/index.php/intel_graphics
cat <<EOF > /usr/share/X11/xorg.conf.d/20-intel.conf
Section "Device"
    Identifier "Intel Graphics"
    Driver "Intel"
    Option "AccelMethod" "uxa"
    Option "DRI" "2"
EndSection
EOF
AS_ROOT
