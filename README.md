# Emacs Configuration

My emacs configuration, mainly targeted at Carbon Emacs. It's focused on being light, simple and consistent with GNU Emacs and Mac OS X's normal functionality with an emphasis on dynamic languages.

## Installation

1. Install Emacs, version >= 22.
   On the Mac, I suggest [Carbon Emacs](http://homepage.mac.com/zenitani/emacs-e.html)
   Otherwise, use your OS's available package manager.
2. Clone the my emacs repository and run `symlinker.sh` to symlink the `.emacs.d` directory in your home folder.

        git clone git://github.com:andrewsardone/emacs.git
        ./symlinker.sh

## TODO

## ELPA

To get a list of the available packages, you can use `M-x package-list-packages;` ELPA will download the most recent list in a buffer. Now, to install packages, move your cursor to them and press i. This will mark the packages for installation. When you're done with marking, press x, and ELPA will install the packages for you (under ~/.emacs.d/elpa/).

