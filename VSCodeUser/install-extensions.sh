#!/bin/sh

set -ue

cd "`dirname "$0"`"
while read ext; do code --install-extension $ext; done < extensions.list
