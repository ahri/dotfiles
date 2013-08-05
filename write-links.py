#!/usr/bin/env python
# coding: utf-8

import os
import re
from shutil import rmtree
from subprocess import call

THIS_FILE = os.path.abspath(__file__)
BASE = os.path.dirname(os.path.abspath(__file__)) + os.sep

def home_linux():
    return os.environ['HOME']

def home_windows():
    return os.environ['USERPROFILE']

def get_home():
    try:
        home = home_linux()
        # msys helpfully gives us c:/foo style paths, which I prefer not to use
        if re.match(r'^[a-z]\:/', home):
            return home_windows()
        return home
    except KeyError:
        return os.environ['USERPROFILE']

HOME = get_home() + os.sep
esc_base = re.escape(BASE)

IGNORE = re.compile(r'|'.join([re.escape(THIS_FILE),
                               esc_base + r'\.git$',
                               esc_base + r'\.gitmodules$',
                               esc_base + r'\.gitignore$',
                               esc_base + r'README',
                               esc_base + r'.*\.swp']))

def force_link(source, target):
    if os.path.isdir(target):
        rmtree(target)
    else:
        os.remove(target)

    try:
        os.symlink(source, target)
    except AttributeError:
        if os.path.isdir(source):
            call(['mklink', '/j', target, source], shell=True)
        else:
            call(['mklink', '/h', target, source], shell=True)
    except OSError:
        print "Could not symlink %s" % target

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
