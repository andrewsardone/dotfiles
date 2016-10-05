#!/usr/bin/env bash

set -e

echo ""
echo "### Installing homebrew-cask apps"
echo ""

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Check for Homebrew
sh $DIR/install-homebrew.sh

apps=(
  ./Casks/vimr.rb
  acorn
  android-studio
  appcleaner
  caffeine
  colorsnapper
  daisydisk
  docker
  fantastical
  firefox
  flux
  framer-studio
  genymotion
  gifs
  google-chrome
  hockey
  imageoptim
  iterm2
  java
  karabiner
  keyboard-cleaner
  kinematic-beta
  licecap
  macdown
  microsoft-office
  nvalt
  omnioutliner
  omnipresence
  postico
  qlmarkdown
  qlprettypatch
  qlstephen
  quicklook-json
  quicksilver
  rescuetime
  rowanj-gitx
  screenflow4
  screenhero
  sequel-pro
  sketch
  sketch-tool
  sketch-toolbox
  skype
  slack
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
