#!/usr/bin/env bash

set -e

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

function is_symlink() {
  if [[ -L "$1" && -d "$1" ]]; then
    return 0
  else
    return 1
  fi
}

OSX_SERVICES_DIR=~/Library/Services

if ! is_symlink $OSX_SERVICES_DIR; then
  echo "Backing up current ~/Library/Services"
  sudo mv $OSX_SERVICES_DIR $OSX_SERVICES_DIR.bak-$(uuidgen)
  ln -s "${DIR}/Services" $OSX_SERVICES_DIR
else
  echo "Services already installed"
fi

echo "Services OK"
