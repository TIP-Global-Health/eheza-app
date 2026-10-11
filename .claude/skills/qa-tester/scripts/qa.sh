#!/bin/bash
# Start, drive and stop the QA driver (driver/driver.ts).
#
#   qa.sh start [--fresh] [--watch]  open the browser; --fresh: empty profile, --watch: visible window
#   qa.sh run [file] [--shot]        run JS from a file, or stdin, as (page, h, qa) => ...
#   qa.sh state [--shot]             summary of the current screen
#   qa.sh record start <pr> <name>   start a real-time recording
#   qa.sh record stop                write client/qa-recordings/<pr>/qa-<pr>-<name>.mp4
#   qa.sh frame <pr> <name> <sec>    save the video's frame at <sec> as a PNG to look at
#   qa.sh stop                       close the browser
#
# Replies are JSON. A reply with "shot" names a PNG of the screen to look at.

set -eu

skill=$(cd "$(dirname "$0")/.." && pwd)
repo=$(cd "$skill/../../.." && pwd)
client="$repo/client"
state="$client/qa-recordings/.driver"
port=${QA_PORT:-9323}
url="http://127.0.0.1:$port"

alive() { curl -s -m 2 -o /dev/null "$url/state"; }

call() { # method path [curl args...]
  local method=$1 route=$2
  shift 2
  alive || { echo "driver not running: qa.sh start" >&2; exit 1; }
  local auth=()
  [ -f "$state/token" ] && auth=(-H "Authorization: Bearer $(cat "$state/token")")
  curl -s -m "${QA_TIMEOUT:-600}" "${auth[@]}" -X "$method" "$url$route" "$@"
  echo
}

cmd=${1:-}
shift || true
case "$cmd" in
  start)
    if alive; then
      echo "driver already running on $url"
      exit 0
    fi
    mkdir -p "$state"
    watch=
    for arg in "$@"; do
      case "$arg" in
        --fresh) rm -rf "$state/profile" "$state/last-url" ;;
        --watch) watch=1 ;;
      esac
    done
    cd "$client"
    # NODE_PATH lets driver.ts, which lives outside client/, import @playwright/test.
    NODE_PATH="$client/node_modules" QA_PORT=$port QA_WATCH=$watch setsid nohup \
      ./node_modules/.bin/playwright test --config "$skill/driver/qa.config.ts" \
      >"$state/driver.log" 2>&1 </dev/null &
    # setsid makes the driver its own process group, so stop can end the browser with it.
    echo $! >"$state/driver.pid"
    for _ in $(seq 60); do
      alive && { echo "driver running on $url (log: $state/driver.log)"; exit 0; }
      sleep 1
    done
    echo "driver did not start; log follows" >&2
    tail -30 "$state/driver.log" >&2
    exit 1
    ;;
  run)
    shot=0
    file=-
    for arg in "$@"; do
      if [ "$arg" = "--shot" ]; then shot=1; else file=$arg; fi
    done
    call POST "/run?shot=$shot" --data-binary "@$file"
    ;;
  state)
    shot=0
    [ "${1:-}" = "--shot" ] && shot=1
    call GET "/state?shot=$shot"
    ;;
  record)
    case "${1:-}" in
      start)
        pr=${2:?usage: qa.sh record start <pr> <name>}
        name=${3:?usage: qa.sh record start <pr> <name>}
        call POST "/record/start?output=$client/qa-recordings/$pr/qa-$pr-$name.mp4"
        ;;
      stop) call POST /record/stop ;;
      *) echo "usage: qa.sh record start <pr> <name> | record stop" >&2; exit 1 ;;
    esac
    ;;
  frame)
    pr=${1:?usage: qa.sh frame <pr> <name> <seconds>}
    name=${2:?usage: qa.sh frame <pr> <name> <seconds>}
    at=${3:?usage: qa.sh frame <pr> <name> <seconds>}
    mkdir -p "$state/shots"
    png="$state/shots/frame-qa-$pr-$name-at-$at.png"
    ffmpeg -y -loglevel error -ss "$at" -i "$client/qa-recordings/$pr/qa-$pr-$name.mp4" \
      -frames:v 1 "$png"
    echo "$png"
    ;;
  stop)
    # A command still running holds the queue, so /stop may not answer; then kill it.
    if alive && QA_TIMEOUT=10 call POST /stop | grep -q '"ok": true'; then
      rm -f "$state/driver.pid"
      echo "driver stopped"
      exit 0
    fi
    if [ -f "$state/driver.pid" ]; then
      kill -- -"$(cat "$state/driver.pid")" 2>/dev/null && echo "driver killed"
      rm -f "$state/driver.pid"
    else
      echo "driver not running"
    fi
    ;;
  *)
    sed -n '2,13p' "$0" >&2
    exit 1
    ;;
esac
