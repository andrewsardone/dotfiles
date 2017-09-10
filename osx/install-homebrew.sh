#!/usr/bin/env bash

set -e

if [ "$(uname)" != "Darwin" ]; then
  echo "Skipping install-homebrew because not macOS"
  exit 0
fi

if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew tap caskroom/cask
brew tap caskroom/fonts
brew tap caskroom/versions
brew tap homebrew/bundle

brew bundle
