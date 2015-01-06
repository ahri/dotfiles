#!/bin/sh

set -ue

if [ ! "`whoami`" = "root" ]; then
    echo "ERROR: run as root" 1>&2
    exit 1
fi

# LED flashing is annoying
grep -qv led_mode < /etc/modprobe.d/iwlwifi.conf && echo "options iwlwifi led_mode=1" >> /etc/modprobe.d/iwlwifi.conf

# Apparently yields battery-life benefits, or maybe freezes...
grep -q pcie_aspm < /etc/default/grub || (awk '/^GRUB_CMDLINE_LINUX/ { sub(/"$/, " pcie_aspm=force\""); } { print; }' < /etc/default/grub > /tmp/__grub && mv /tmp/__grub /etc/default/grub && grub2-mkconfig)

# Enable the brightness function keys
[ ! -e /etc/X11/xorg.conf.d/20-intel.conf ] && cat <<EOF > /etc/X11/xorg.conf.d/20-intel.conf
Section "Device"
    Identifier  "Intel Graphics"
    Driver      "intel"
    BusID       "PCI:0:2:0"
    Option      "Backlight"       "intel_backlight"
EndSection
EOF
