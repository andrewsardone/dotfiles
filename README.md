# Emacs Configuration

This is my emacs configuration, mainly targeted at Carbon Emacs. It's focused on being light, simple and consistent with GNU Emacs and Mac OS X's normal functionality with an emphasis on dynamic languages.

## Installation

1. Install Emacs, version >= 22.
   On the Mac, I suggest downloading the [latest source](http://ftp.gnu.org/pub/gnu/emacs/) and building (see nextstep/INSTALL for Mac OS X instructions), or grabbing a [nightly build](http://atomized.org/wp-content/cocoa-emacs-nightly/) (more cutting edge).

   Otherwise, use your OS's available package manager.
2. Clone the my emacs repository and run `symlinker.sh` to symlink the `.emacs.d` directory in your home folder.

        git clone git://github.com/andrewsardone/emacs.git
        ./install

3. I use the [Rinari](http://rinari.rubyforge.org/), a Ruby on Rails Minor Mode for Emacs, and have it included as a git submodule. You'll need pull down the Rinari submodule and its submodules (jump.el and ert.git)

        git submodule init
        git submodule update
        cd emacs.d/vendor/rinari
        git submodule init
        git submodule update

## TODO

- Fix `/bin/bash: markdown: command not found` on `markdown-preview`

## ELPA

To get a list of the available packages, you can use `M-x package-list-packages;` ELPA will download the most recent list in a buffer. Now, to install packages, move your cursor to them and press i. This will mark the packages for installation. When you're done with marking, press x, and ELPA will install the packages for you (under ~/.emacs.d/elpa/).

