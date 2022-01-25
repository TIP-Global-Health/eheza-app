#!/bin/sh
set -e

# ---------------------------------------------------------------------------- #
#
# Install server dependencies.
#
# ---------------------------------------------------------------------------- #

# Build our own Docker image based on https://github.com/Gizra/drupal-lamp.
cd /home/circleci/project
docker build -t server -f ci-scripts/docker_files/Dockerfile .

# Simple Docker run, no need for Zalenium dependencies.
if [ -z "${BUILD_WEBDRIVERIO+x}" ]; then
  exit 0;
fi

# Zalenium requires to download this dependency image first.
docker pull elgalu/selenium:3.7
