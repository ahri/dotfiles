#!/bin/sh

set -ue

if [ $# -lt 1 ]; then
	echo "Specify a script name" 1>&2
	exit 1
fi

script_path="$1"
shift
script_dir="`dirname "$script_path"`"
script_name="`basename "$script_path"`"

cd "$script_dir"

cmd=`awk '
BEGIN { x = 0; cmd = "" }

x == 1 {
	cmd = cmd " " $0
}

/^{- stack/ {
	x = 1;
	sub(/^{- /, "");
	sub(/runghc/, "ghci")
	cmd = $0
}

/-}/ { x = 0 }

END { 
	sub(/-}/, "", cmd)
	print cmd
}
' < "$script_name"`

$cmd "$@" "$script_name"
