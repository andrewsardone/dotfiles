#!/usr/bin/env bash

set -e

# Check for Homebrew
if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

apps=(
  android-studio
  appcleaner
  bowtie
  doxie
  firefox
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
