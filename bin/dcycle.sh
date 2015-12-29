#!/bin/sh

name=$1

dc.sh "kill $name" "rm -f $name" "up -d"
