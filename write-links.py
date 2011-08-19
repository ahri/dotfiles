#!/usr/bin/env python
# coding: utf-8

import os
import re
from shutil import rmtree

THIS_FILE = os.path.abspath(__file__)
BASE = os.path.dirname(os.path.abspath(__file__)) + os.sep
HOME = os.environ['HOME'] + os.sep
esc_base = re.escape(BASE)

IGNORE = re.compile(r'|'.join([re.escape(THIS_FILE),
                               esc_base + r'\.git',
                               esc_base + r'\.gitignore',
                               esc_base + r'README',
                               esc_base + r'.*\.swp']))

def force_link(source, target):
    if os.path.islink(target):
        os.remove(target)
    else:
        rmtree(target, True)

    os.symlink(source, target)

def linkify(subpath):
    based = BASE + subpath
    homed = HOME + subpath

    if IGNORE.match(based) is not None:
        return

    # 1. if we have a straight file, just force link it
    if not os.path.isdir(BASE + subpath):
        force_link(based, homed)
        return

    # 2. if we have a dir that doesn't exist as a dir in the homedir, force link it
    if not os.path.exists(homed):
        force_link(based, homed)
        return

    # 3. if the dir already exists recurse per item in base dir/subpath
    for item in os.listdir(based):
        # recurse
        linkify(subpath + os.sep + item)

if __name__ == '__main__':
    for item in os.listdir(BASE):
        linkify(item)
