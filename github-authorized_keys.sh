#!/bin/sh

set -ue

if [ $# -ne 1 ]; then
    echo "ERROR: provide a github username" >&2
    exit 1
fi

github_user="$1"

keys="`wget https://github.com/$github_user.keys -qO-`"

if [ `echo -n "$keys" | wc -l` -gt 0 ]; then
    echo "$keys" > $HOME/.ssh/authorized_keys
fi
