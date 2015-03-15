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

# Install other useful binaries
binaries=(
  ack
  ctags
  git
  git-extras  grc
  grc
  hub
  mackup
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
