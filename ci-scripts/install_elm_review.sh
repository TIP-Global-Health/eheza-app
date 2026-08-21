#!/bin/sh
set -e

# ---------------------------------------------------------------------------- #
#
# Install Elm Review dependencies.
#
# ---------------------------------------------------------------------------- #

# Install Elm compiler (required by elm-review)
# Using the official binary release for Linux x64
ELM_VERSION=0.19.2
wget -q "https://github.com/elm/compiler/releases/download/${ELM_VERSION}/elm-${ELM_VERSION}-linux-x64.gz"
gunzip "elm-${ELM_VERSION}-linux-x64.gz"
chmod +x "elm-${ELM_VERSION}-linux-x64"
sudo mv "elm-${ELM_VERSION}-linux-x64" /usr/local/bin/elm

echo "Elm compiler installed successfully"