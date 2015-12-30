#!/bin/sh

name=$1

dc.sh "kill $name" "rm -vf $name" "up -d"
