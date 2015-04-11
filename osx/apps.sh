#!/usr/bin/env bash

set -e

echo ""
echo "### Installing homebrew-cask apps"
echo ""

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Check for Homebrew
sh $DIR/install-homebrew.sh

apps=(
  acorn
  android-studio
  appcleaner
  bee
  bowtie
  caffeine
  doxie
  fantastical
  firefox
  fluid
  flux
  framer-studio
  genymotion
  google-chrome
  hockey
  imageoptim
  iterm2
  java7
  karabiner
  keyboard-cleaner
  macvim
  nvalt
  omnioutliner
  omnipresence
  postico
  qlmarkdown
  qlprettypatch
  qlstephen
  quicklook-json
  quicksilver
  rowanj-gitx
  screenflow
  screenhero
  sequel-pro
  sketch-tool
  sketch-toolbox
  skype
  slate
  spotify
  superduper
  vagrant
  virtualbox
  viscosity
  vlc
)

echo "installing cask..."
brew install caskroom/cask/brew-cask

echo "installing alternate cask version"
brew tap caskroom/versions

echo "installing apps..."
brew cask install ${apps[@]}

brew cleanup

exit 0
