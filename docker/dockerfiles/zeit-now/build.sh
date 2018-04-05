#!/usr/bin/env bash

set -e
set -x

docker build \
  -t andrewsardone/docker-zeit-now:latest \
  -t andrewsardone/docker-zeit-now:v11.0.3 \
  .
