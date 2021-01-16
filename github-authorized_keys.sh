#!/bin/sh

set -ue

if [ $# -eq 0 ]; then
    echo "ERROR: provide one or more github usernames" >&2
    exit 1
fi

keys=""
while [ $# -gt 0 ]; do
    github_user="$1"
    keys="$keys`curl -s https://github.com/$github_user.keys | sed "s/$/ github.com\/$github_user/"`"
    shift
done

if [ `echo -n "$keys" | wc -l` -gt 0 ]; then
    echo "$keys" > $HOME/.ssh/authorized_keys
fi
