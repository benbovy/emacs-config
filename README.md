emacs-config
============

My emacs configuration.


Installation
------------

- install emacs, e.g., using brew:

    $ brew cask install emacs

- install [Prelude](https://github.com/bbatsov/prelude).

- run the install script that symlink emacs configuration files:

    $ python install.py

- there is also a plist file to setup a service on MacOS with, e.g.,
  [launchrocket](https://github.com/jimbojsb/launchrocket)
  (unfortunately, the emacs cask doesn't support `brew services`).


See Also
--------

A lot of customization on top of prelude is inspired by
[Terencio's config](https://github.com/rememberYou/.emacs.d).
