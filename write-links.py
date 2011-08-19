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
        print target
        print 'blah'
        rmtree(target, True)

    os.symlink(source, target)

def linkify(subpath):
    based = BASE + subpath
    homed = HOME + subpath

    if IGNORE.match(based) is not None:
        return

    print 'OPERATING ON:', subpath

    if not os.path.isdir(based):
        force_link(based, homed)
        return

    if not os.path.isdir(homed) or os.path.islink(homed):
        force_link(based, homed)
        return

    for item in os.listdir(based):
        # recurse
        linkify(subpath + os.sep + item)

if __name__ == '__main__':
    for item in os.listdir(BASE):
        linkify(item)
