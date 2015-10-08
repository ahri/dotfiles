#!/bin/sh

set -ue

name=`cat NAME`
version=`cat VERSION`

tag="$name:$version"
docker build -t $tag .
docker tag -f $tag $name:latest
