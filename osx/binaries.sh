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
  hub
  mackup
  node
  ruby
  ruby-install
  sqlite
  tig
  tmux
  tree
  watchman
  wget
  xctool
)

# Install the binaries
brew install ${binaries[@]}

# Remove outdated versions from the cellar
brew cleanup

exit 0
