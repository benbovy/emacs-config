# -*- coding: utf-8 -*-
"""
Install script to copy (symlink) emacs configuration files.

"""

import os
import inspect


USER_DIR = os.path.expanduser('~')
THIS_DIR = os.path.dirname(
    os.path.abspath(inspect.getfile(inspect.currentframe()))
)
EMACS_CONF_DIR = os.path.join(USER_DIR, ".emacs.d")
EMACS_PERS_DIR = os.path.join(EMACS_CONF_DIR, "personal")

for d in (EMACS_CONF_DIR, EMACS_PERS_DIR):
    try:
        os.mkdir(d)
    except OSError:
        pass

try:
    os.symlink(os.path.join(THIS_DIR, 'myconf.el'),
               os.path.join(EMACS_PERS_DIR, 'myconf.el'))
except FileExistsError:
    pass

try:
    os.symlink(os.path.join(THIS_DIR, 'prelude-modules.el'),
               os.path.join(EMACS_CONF_DIR, 'prelude-modules.el'))
except FileExistsError:
    pass
