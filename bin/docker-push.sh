#!/bin/sh

set -ue

name=`cat NAME`
version=`cat VERSION`

tag="$name:$version"

docker push $tag
docker push $name:latest
