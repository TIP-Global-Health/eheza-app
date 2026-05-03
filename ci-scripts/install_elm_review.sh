#!/bin/sh
set -e

# ---------------------------------------------------------------------------- #
#
# Install Elm Review dependencies.
#
# ---------------------------------------------------------------------------- #

# Install Elm compiler (required by elm-review)
# Using the official binary release for Linux x64
wget -q https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
gunzip binary-for-linux-64-bit.gz
chmod +x binary-for-linux-64-bit
sudo mv binary-for-linux-64-bit /usr/local/bin/elm

echo "Elm compiler installed successfully"