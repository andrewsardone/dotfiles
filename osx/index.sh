#!/usr/bin/env bash

set -e

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

if [ "$(uname)" != "Darwin" ]; then
  echo "Skipping macOS setup because not on macOS"
  exit 0
fi

sh $DIR/osx-defaults
sh $DIR/install-homebrew.sh
sh $DIR/services/install.sh
