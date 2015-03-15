#!/usr/bin/env bash

set -e

# Check for Homebrew
if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Update homebrew
brew update

# Install other useful binaries
binaries=(
  ack
  ctags
  git
  git-extras  grc
  grc
  hub
  node
  ruby
  sqlite
  tig
  tmux
  tree
  wget
)

# Install the binaries
brew install ${binaries[@]}

# Remove outdated versions from the cellar
brew cleanup

exit 0
