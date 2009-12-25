# Emacs Configuration

This is my emacs configuration, targeted at the NeXTSTEP/Cocoa Emacs not built on top of Aqua. It's focused on being light, simple and consistent with an emphasis on dynamic languages.

## Installation

1. Install Emacs, version >= 23.
   I suggest you use [Homebrew](http://github.com/mxcl/homebrew) and install the included `emacs` formula.

   Otherwise, use your OS's available package manager.
2. Clone the my emacs repository and run `install` to symlink the `.emacs.d` directory in your home folder.

        git clone git://github.com/andrewsardone/emacs-config.git
        ./install

3. I use the [Rinari](http://rinari.rubyforge.org/), a Ruby on Rails Minor Mode for Emacs, and have it included as a git submodule. You'll need pull down the Rinari submodule and its submodules (jump.el and ert.git)

        git submodule init
        git submodule update
        cd emacs.d/vendor/rinari
        git submodule init
        git submodule update

## TODO

- Fix `/bin/bash: markdown: command not found` on `markdown-preview`

