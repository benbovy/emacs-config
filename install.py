# -*- coding: utf-8 -*-
"""
Install script to copy (symlink) emacs configuration files.

"""

import os
import glob
import inspect


USER_DIR = os.path.expanduser('~')
THIS_DIR = os.path.dirname(
    os.path.abspath(inspect.getfile(inspect.currentframe()))
)
EMACS_CONF_DIR = os.path.join(USER_DIR, ".emacs.d")
EMACS_PERS_DIR = os.path.join(EMACS_CONF_DIR, "personal")
EMACS_PRELOAD_DIR = os.path.join(EMACS_PERS_DIR, "preload")

for d in (EMACS_CONF_DIR, EMACS_PERS_DIR, EMACS_PRELOAD_DIR):
    try:
        os.mkdir(d)
    except OSError:
        pass

for elf in glob.iglob("dotemacs/**/*.el", recursive=True):
    elf_target = os.path.join(THIS_DIR, elf)
    elf_link = os.path.join(USER_DIR,
                            elf.replace("dotemacs", ".emacs.d"))
    try:
        os.symlink(elf_target, elf_link)
        print("link " + elf_link + " -> " + elf_target)
    except FileExistsError:
        print("skip " + elf)
