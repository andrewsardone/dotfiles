## Installation

    git clone https://github.com/andrewsardone/dotfiles.git ~/.dotfiles
    cd ~/.dotfiles
    make

### Mac setup

Inspired by [this post][tp], the `make mac` task will set up *a lot* of my
default Mac OS X configuration.

- Install and sync [Dropbox](https://www.dropbox.com/)
- Install Xcode from the Mac App Store
- Run through the [App Store installations][asi]
- Clone the project and run the `make mac` task:

```sh
git clone https://github.com/andrewsardone/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
make mac
mackup backup # optional if you want to use https://github.com/lra/mackup
```

[tp]: http://lapwinglabs.com/blog/hacker-guide-to-setting-up-your-mac
[asi]: osx/app-store-apps.md
