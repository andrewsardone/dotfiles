#!/usr/bin/env bash

set -e

echo ""
echo "### Installing homebrew-cask apps"
echo ""

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Check for Homebrew
sh $DIR/install-homebrew.sh

apps=(
  android-studio
  appcleaner
  bowtie
  doxie
  firefox
  fluid
  flux
  framer-studio
  genymotion
  google-chrome
  imageoptim
  iterm2
  karabiner
  keyboard-cleaner
  macvim
  nvalt
  qlmarkdown
  qlprettypatch
  qlstephen
  quicklook-json
  quicksilver
  rowanj-gitx
  screenflow
  sequel-pro
  skype
  slate
  spotify
  vagrant
  virtualbox
  viscosity
  vlc
)

echo "installing cask..."
brew tap phinze/homebrew-cask
brew install brew-cask

echo "installing apps..."
brew cask install ${apps[@]}

brew cleanup

exit 0
