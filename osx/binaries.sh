#!/usr/bin/env bash

set -e

echo ""
echo "### Installing homebrew binaries"
echo ""

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Check for Homebrew
sh $DIR/install-homebrew.sh

# Update homebrew
brew update

# Custom taps
brew tap homebrew/binary

# Install other useful binaries
binaries=(
  ack
  ag
  aws-elasticbeanstalk
  awscli
  chruby
  ctags
  dateutils
  exercism
  ffmpeg
  fpp
  gifsicle
  git
  git-extras
  grc
  htop
  hub
  mackup
  neovim/neovim/neovim
  node
  ruby
  ruby-install
  sqlite
  tig
  tmate
  tmux
  tree
  vim
  watchman
  wget
  xctool
)

# Install the binaries
brew install ${binaries[@]}

# Remove outdated versions from the cellar
brew cleanup

exit 0
