#!/bin/bash
set -euo pipefail

# Checks re-creation of every content type a device can report missing.
# Usage: test_sync_incident_recreation.sh [build|run], both by default.

cd "$(dirname "$0")/.."
CHECK_DIR=server/hedley/modules/custom/hedley_user/tests/sync_incident_recreation
BUILD_DIR=/tmp/sync-incident-recreation

# From a git worktree, set DDEV_DIR to the checkout DDEV runs from, or DDEV
# starts a second project for the worktree.
DDEV_DIR=${DDEV_DIR:-$PWD}

# Compiles the Elm worker. Needs no DDEV, so CI runs it before DDEV starts.
build () {
  rm -rf "$BUILD_DIR"
  mkdir -p "$BUILD_DIR/elm/src"
  cp "$CHECK_DIR/SyncIncidentWorker.elm" "$BUILD_DIR/elm/src/"
  # The client's elm.json, reading the client's sources from their real place.
  node -e '
    const fs = require("fs");
    const [client, out] = process.argv.slice(1);
    const project = JSON.parse(fs.readFileSync(client + "/elm.json"));
    project["source-directories"] = ["src", client + "/src/elm"];
    fs.writeFileSync(out, JSON.stringify(project, null, 4));
  ' "$PWD/client" "$BUILD_DIR/elm/elm.json"
  (cd "$BUILD_DIR/elm" && elm make src/SyncIncidentWorker.elm --output="$BUILD_DIR/worker.js")
}

# Runs the check in the DDEV web container.
run () {
  if [ ! -f "$BUILD_DIR/worker.js" ]; then
    echo "$BUILD_DIR/worker.js is missing. Run: $0 build"
    exit 1
  fi
  cp "$CHECK_DIR/check.php" "$CHECK_DIR/run.js" "$BUILD_DIR/"
  # Copied in, as DDEV_DIR may be another checkout than this one.
  tar cf - -C "$BUILD_DIR" check.php run.js worker.js |
    (cd "$DDEV_DIR" && ddev exec "rm -rf $BUILD_DIR && mkdir -p $BUILD_DIR && tar xf - -C $BUILD_DIR")
  (cd "$DDEV_DIR" && ddev exec drush php-script "$BUILD_DIR/check.php")
}

case "${1:-}" in
  build) build ;;
  run) run ;;
  "") build && run ;;
  *) echo "Usage: $0 [build|run]"; exit 1 ;;
esac
