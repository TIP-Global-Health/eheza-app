#!/bin/bash

# ---------------------------------------------------------------------------- #
#
# Rebuild the server Elm app and compare it to the committed bundle.
#
# The admin app is compiled by hand, and its output is committed as
# server/hedley/modules/custom/hedley_general/js/elm-main.js. The build is
# deterministic and not optimized, so the rebuilt output must match the
# committed file byte for byte; a difference means the bundle was not
# regenerated after a source change.
#
# ---------------------------------------------------------------------------- #

set -e

cd server/elm || exit 1

BUNDLE=../hedley/modules/custom/hedley_general/js/elm-main.js
REBUILT=$(mktemp --suffix=.js)

elm make src/Main.elm --output "$REBUILT"

if cmp -s "$REBUILT" "$BUNDLE"; then
  echo "✓ The committed server Elm bundle matches its source"
  rm -f "$REBUILT"
else
  echo "✗ The committed server Elm bundle differs from its source."
  echo "  Rebuild it from server/elm and commit the result:"
  echo "  elm make src/Main.elm --output ../hedley/modules/custom/hedley_general/js/elm-main.js"
  rm -f "$REBUILT"
  exit 1
fi
