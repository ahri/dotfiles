#!/bin/sh

set -ue

while [ $# -gt 0 ]; do
    docker-compose $1
    shift
done
