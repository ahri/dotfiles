#!/bin/sh

set -ue

if [ ! "`whoami`" = "root" ]; then
    echo "ERROR: run as root" 1>&2
    exit 1
fi

# LED flashing is annoying
([ ! -e /etc/modprobe.d/iwlwifi.conf ] || grep -qv led_mode < /etc/modprobe.d/iwlwifi.conf) && echo "options iwlwifi led_mode=1" >> /etc/modprobe.d/iwlwifi.conf

# Apparently yields battery-life benefits, or maybe freezes...
grep -q pcie_aspm < /etc/default/grub || (awk '/^GRUB_CMDLINE_LINUX/ { sub(/"$/, " pcie_aspm=force\""); } { print; }' < /etc/default/grub > /tmp/__grub && mv /tmp/__grub /etc/default/grub && grub2-mkconfig -o /boot/grub2/grub.cfg)

# Enable the brightness function keys
grep -q acpi_osi < /etc/default/grub || (awk '/^GRUB_CMDLINE_LINUX/ { sub(/"$/, " acpi_osi=\""); } { print; }' < /etc/default/grub > /tmp/__grub && mv /tmp/__grub /etc/default/grub && grub2-mkconfig -o /boot/grub2/grub.cfg)

# Nicer font rendering
cd /etc/fonts/conf.d
ln -sf /usr/share/fontconfig/conf.avail/10-autohint.conf
ln -sf /usr/share/fontconfig/conf.avail/11-lcdfilter-default.conf
