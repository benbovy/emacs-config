# -*- coding: utf-8 -*-
"""
Install script to copy (symlink) emacs configuration files.

"""

import os
import shutil
import inspect


USER_DIR = os.path.expanduser('~')
THIS_DIR = os.path.dirname(
    os.path.abspath(inspect.getfile(inspect.currentframe()))
)
EMACS_CONF_DIR = os.path.join(USER_DIR, ".emacs.d")

try:
    os.mkdir(EMACS_CONF_DIR)
except OSError:
    pass

os.symlink(os.path.join(THIS_DIR, 'myconf.el'),
           os.path.join(EMACS_CONF_DIR, 'personal', 'myconf.el'))

#
#shutil.copy(os.path.join(THIS_DIR, 'myconf.el'),
#            os.path.join(EMACS_CONF_DIR, 'personal', 'myconf.el'))
