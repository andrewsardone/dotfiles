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
  ansible
  aws-elasticbeanstalk
  awscli
  ctags
  dateutils
  exercism
  exif
  ffmpeg
  fpp
  fzf
  gifsicle
  git
  git-extras
  gpg
  grc
  htop
  hub
  mackup
  mas
  node
  ruby
  spark
  speedtest_cli
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
