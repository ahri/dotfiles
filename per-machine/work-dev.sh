#!/bin/sh

set -ue

rsync --archive --delete --include '*/' --include '**/.git/**' --exclude '*' --prune-empty-dirs /Volumes/AdamVmHome/.vim ~/
find ~/.vim -type d -name .git -exec bash -c "cd {} && cd .. && pwd && git checkout ." \;
