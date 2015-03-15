#!/usr/bin/env bash

set -e

if [ "$(uname)" == "Darwin" ] && test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
