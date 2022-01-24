#!/bin/sh
set -e

# ---------------------------------------------------------------------------- #
#
# Install Elm Format.
#
# ---------------------------------------------------------------------------- #

wget https://github.com/avh4/elm-format/releases/download/0.8.5/elm-format-0.8.5-linux-x64.tgz
tar xfz elm-format-0.8.5-linux-x64.tgz
sudo mv elm-format /usr/bin/
