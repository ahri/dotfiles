#!/bin/sh
script="`readlink -f "$0"`"
dir="`dirname "$script"`"

find "$dir" -maxdepth 1 | while read file; do

        case "$file" in
        "$dir"|"$dir/.git"|"$dir/.gitignore"|"$dir/README"|*.swp|"$0")
                continue
                ;;
        esac

        rm -rf "$HOME/`basename $file`"
        ln -s "$file" "$HOME"
done
