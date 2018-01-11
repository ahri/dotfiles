#!/bin/sh

set -ue

cd "`dirname "$0"`"
code --list-extensions | sort -u > extensions.list
