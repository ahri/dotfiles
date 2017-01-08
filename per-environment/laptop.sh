#!/bin/sh

set -ue

if [ ! "`whoami`" = "root" ]; then
    echo "ERROR: run as root" 1>&2
    exit 1
fi

# LED flashing is annoying
([ ! -e /etc/modprobe.d/iwlwifi.conf ] || grep -qv led_mode < /etc/modprobe.d/iwlwifi.conf) && echo "options iwlwifi led_mode=1" >> /etc/modprobe.d/iwlwifi.conf

# Apparently yields battery-life benefits, or maybe freezes...
grep -q pcie_aspm < /etc/default/grub || (awk '/^GRUB_CMDLINE_LINUX/ { sub(/"$/, " pcie_aspm=force\""); } { print; }' < /etc/default/grub > /tmp/__grub && mv /tmp/__grub /etc/default/grub && (grub-mkconfig || grub2-mkconfig))

# Nicer font rendering
cd /etc/fonts/conf.d
ln -sf /usr/share/fontconfig/conf.avail/10-autohint.conf
ln -sf /usr/share/fontconfig/conf.avail/11-lcdfilter-default.conf

# note: "syndaemon -i 1.0 -K -R -t" for laptop

cat <<EOF > /usr/share/X11/xorg.conf.d/20-intel.conf
Section "Device"
    Identifier "Intel Graphics"
    Driver "Intel"
    Option "AccelMethod" "sna"
    Option "TearFree" "true"
    Option "TripleBuffer" "true"
    Option "MigrationHeuristic" "greedy"
    Option "Tiling" "true"
    Option "Pageflip" "true"
    Option "ExaNoComposite" "false"
    Option "Tiling" "true"
    Option "Pageflip" "true"
    Option "DRI" "3"
EndSection
EOF
