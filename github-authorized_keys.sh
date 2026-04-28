#!/bin/sh

set -ue

if [ $# -eq 0 ]; then
    echo "ERROR: provide one or more github usernames" >&2
    exit 1
fi

authorized_keys="$HOME/.ssh/authorized_keys"
new_keys=""

while [ $# -gt 0 ]; do
    github_user="$1"
    keys=$(curl -s "https://github.com/$github_user.keys" | sed "s/$/ github.com\/$github_user/")
    new_keys="${new_keys}${keys}
"
    shift
done

old_keys="`cat "$authorized_keys" 2>/dev/null || true`"

# avoid disk write
if [ ! "$new_keys" = "$old_keys" ]; then
    temp_file="`mktemp`"
    echo "$new_keys" > "$temp_file"
    # atomic replace
    mv "$temp_file" "$authorized_keys"
fi
