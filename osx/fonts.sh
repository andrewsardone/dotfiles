#!/usr/bin/env bash

set -e

echo ""
echo "### Installing homebrew-cask fonts"
echo ""

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Check for Homebrew
sh $DIR/install-homebrew.sh

fonts=(
  font-blokk
  font-kreon
  font-lato
  font-league-gothic
  font-open-sans
  font-open-sans-condensed
  font-pt-mono
  font-pt-sans
  font-pt-serif
  font-roboto
  font-source-code-pro
)

echo "installing cask..."
brew tap phinze/homebrew-cask
brew install brew-cask

# Tap the fonts
echo "installing caskroom/fonts"
brew tap caskroom/fonts

# install fonts
echo "installing fonts..."
brew cask install ${fonts[@]}

brew cleanup

exit 0
