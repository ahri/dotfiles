#!/bin/sh

set -ue

rsync -rt --delete --include '*/' --include '**/.git/**' --exclude '*' --prune-empty-dirs -e 'ssh -l user' linux:.vim ~/
find ~/.vim -type d -name .git -exec bash -c "cd {} && cd .. && pwd && git checkout ." \;
