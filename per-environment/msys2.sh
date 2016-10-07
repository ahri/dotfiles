#!/bin/sh

set -ue

pacman -S vim git base-devel ruby
grep -q '\s/home\s' /etc/fstab || echo 'C:/Users /home ntfs binary,noacl,auto 1 1' >> /etc/fstab
