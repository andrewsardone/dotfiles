# My Emacs Configuration

Constantly in a state of flux.

Heavily influenced by Bozhidar Batsov's [emacs-prelude](https://github.com/bbatsov/emacs-prelude).

## Requirements

- Emacs 24

### Installation

On Mac OS X, you'll need to grab Emacs 24 since the system's default emacs is old. You could grab one of the nightly builds from [Emacs for Mac OS X](http://emacsformacosx.com/), but I suggest you use homebrew. I also suggest including the `--cocoa` flag to build Emacs with the fullscreen patches.

    brew install emacs --HEAD --use-git-head --cocoa

Then clone the repo to `~/.emacs.d`

    cd ~/
    git clone https://github.com/andrewsardone/emacs-config.git .emacs.d
