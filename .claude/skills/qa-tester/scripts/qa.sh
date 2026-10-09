#!/bin/bash
# Start, drive and stop the QA driver (driver/driver.ts).
#
#   qa.sh start [--fresh]            open the browser; --fresh starts from an empty profile
#   qa.sh run [file] [--shot]        run JS from a file, or stdin, as (page, h, qa) => ...
#   qa.sh state [--shot]             summary of the current screen
#   qa.sh record start <pr> <name>   start a real-time recording
#   qa.sh record stop                write client/qa-recordings/<pr>/qa-<pr>-<name>.mp4
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
    [ "${1:-}" = "--fresh" ] && rm -rf "$state/profile" "$state/last-url"
    cd "$client"
    # NODE_PATH lets driver.ts, which lives outside client/, import @playwright/test.
    NODE_PATH="$client/node_modules" QA_PORT=$port setsid nohup \
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
  stop)
    # A command still running holds the queue, so /stop may not answer; then kill it.
    if alive && QA_TIMEOUT=10 call POST /stop | grep -q '"ok": true'; then
      rm -f "$state/driver.pid"
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
    sed -n '2,12p' "$0" >&2
    exit 1
    ;;
esac
