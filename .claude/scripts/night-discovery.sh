#!/bin/bash
# Spend spare Fable weekly allowance on discovery rounds, unattended.
#
# Cron starts this at 00:00 on Saturday, Sunday, Monday and Tuesday. It reads
# the Fable weekly figure, compares it with that night's ceiling, and launches
# rounds one after another while a whole round still fits under the ceiling.
#
# A round that starts always finishes; only the next launch is decided.
#
#   night-discovery.sh             the cron entry point
#   night-discovery.sh --dry-run   print the decision, launch nothing
#   night-discovery.sh --trial     one round now, whatever the day and ceiling
#
# To restart after a dry-stop, delete the "paused" file in the state directory.

set -u

REPO="${CLAUDE_PROJECT_DIR:-/var/www/html/ihangane}"
STATE="${NIGHT_DISCOVERY_STATE:-$HOME/.local/state/eheza-night-discovery}"
MODEL="${NIGHT_DISCOVERY_MODEL:-claude-fable-5-2}"
SCOPE="Fable"            # The weekly row to read, by its model label.
END_HOUR=7               # No round may still be running after this hour.
SESSION_MAX=90           # Five-hour meter: a round must end below this.
DRY_STOP_AFTER=3         # Consecutive rounds with no new item.
FIRST_ROUND_MINUTES=60   # Assumed length until a round has been measured.
FIRST_ROUND_SESSION=40   # Assumed session cost until one has been measured.

MODE="night"
case "${1:-}" in
  --dry-run) MODE="dry-run" ;;
  --trial) MODE="trial" ;;
  "") ;;
  *) echo "usage: $0 [--dry-run|--trial]" >&2; exit 2 ;;
esac

# Cron runs with a bare PATH; claude lives in ~/.local/bin.
export PATH="$PATH:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin"

mkdir -p "$STATE/rounds"
LOG="$STATE/night.log"
COSTS="$STATE/costs.tsv"
[ -f "$COSTS" ] || printf 'started\tminutes\tweekly_before\tweekly_after\tsession_before\tsession_after\tnew_items\texit\tweekly_cost\tsession_cost\n' > "$COSTS"

log () { printf '%s  %s\n' "$(date '+%F %T')" "$*" | tee -a "$LOG"; }

# One launcher at a time: a round still running when cron fires again is not doubled.
exec 9> "$STATE/lock"
flock -n 9 || { log "another launcher holds the lock; exiting"; exit 0; }

# Prints "<weekly> <session> <session reset, epoch seconds>" or fails.
read_usage () {
  (cd "$STATE" && timeout 120 claude -p "/usage" --output-format stream-json --verbose < /dev/null 2>/dev/null 9>&-) \
    | python3 -c '
import json, sys
from datetime import datetime
scope = sys.argv[1]
weekly = session = reset = None
for line in sys.stdin:
    try:
        limits = json.loads(line)["usage_report"]["rate_limits"]["limits"]
    except (ValueError, KeyError, TypeError):
        continue
    for row in limits or []:
        model = ((row.get("scope") or {}).get("model") or {}).get("display_name")
        if row.get("kind") == "weekly_scoped" and model == scope:
            weekly = row["percent"]
        elif row.get("kind") == "session":
            session = row["percent"]
            if row.get("resets_at"):
                reset = int(datetime.fromisoformat(row["resets_at"]).timestamp())
if weekly is None:
    sys.exit(1)
# An idle five-hour window has no row: nothing of it is used.
print(round(weekly), round(session or 0), reset or 0)
' "$SCOPE"
}

# Largest value of one column over the last three measured rounds.
recent_max () {
  awk -F'\t' -v c="$1" 'NR>1 && $8==0 && $c!="" && $c>=0 {v[++n]=$c} END {m=""; for (i=n; i>n-3 && i>0; i--) if (m=="" || v[i]>m) m=v[i]; print m}' "$COSTS"
}

count_items () { find "$REPO/.claude/backlog/items" -maxdepth 1 -name '*.md' | wc -l; }

# Tonight's ceiling on the weekly figure, by the weekday the run starts on.
# Saturday shares the climb to 50 with Sunday, so it stops halfway there.
ceiling_for_tonight () {
  local weekly="$1" day file
  day=$(date +%u)
  case "$day" in
    6)
      file="$STATE/ceiling-$(date +%F)"
      [ -f "$file" ] || echo $(( weekly < 50 ? weekly + (50 - weekly) / 2 : weekly )) > "$file"
      cat "$file" ;;
    7) echo 50 ;;
    1) echo 70 ;;
    2) echo 95 ;;
    *) echo "" ;;
  esac
}

if [ "$MODE" = "trial" ]; then
  deadline=$(( $(date +%s) + 24 * 3600 ))
else
  deadline=$(date -d "today ${END_HOUR}:00" +%s)
fi
launched=0

while :; do
  if [ -f "$STATE/paused" ] && [ "$MODE" != "trial" ]; then
    log "paused by the dry-stop ($(cat "$STATE/paused")); delete $STATE/paused to restart"
    break
  fi

  branch=$(git -C "$REPO" rev-parse --abbrev-ref HEAD 2>/dev/null)
  if [ "$branch" != "develop" ]; then
    log "main tree is on '$branch', not develop; nothing launched"
    break
  fi

  if ! usage=$(read_usage); then
    log "no '$SCOPE' weekly row in the usage report; nothing launched"
    break
  fi
  read -r weekly session session_reset <<< "$usage"

  ceiling=$(ceiling_for_tonight "$weekly")
  [ "$MODE" = "trial" ] && ceiling=100
  if [ -z "$ceiling" ]; then
    log "not a discovery night (weekly $weekly%); nothing launched"
    break
  fi

  weekly_cost=$(recent_max 9); session_cost=$(recent_max 10); minutes=$(recent_max 2)
  : "${weekly_cost:=1}" "${session_cost:=$FIRST_ROUND_SESSION}" "${minutes:=$FIRST_ROUND_MINUTES}"
  [ "$weekly_cost" -lt 1 ] && weekly_cost=1
  status="weekly $weekly% (ceiling $ceiling%, a round costs up to $weekly_cost), session $session% (a round costs up to $session_cost), a round takes up to $minutes min"

  now=$(date +%s)
  if [ $(( now + minutes * 60 )) -gt "$deadline" ]; then
    log "no time for another round before ${END_HOUR}:00 — $status"
    break
  fi
  if [ $(( weekly + weekly_cost )) -gt "$ceiling" ]; then
    log "ceiling reached — $status"
    break
  fi
  if [ $(( session + session_cost )) -gt "$SESSION_MAX" ]; then
    # Wait for the five-hour window to reset when a round still fits afterwards.
    if [ "$session_reset" -gt "$now" ] && [ $(( session_reset + 120 + minutes * 60 )) -le "$deadline" ]; then
      log "session meter too full; waiting for its reset at $(date -d "@$session_reset" '+%H:%M') — $status"
      [ "$MODE" = "dry-run" ] && break
      sleep $(( session_reset + 120 - now ))
      continue
    fi
    log "session meter too full and no reset in time — $status"
    break
  fi

  log "launching a round — $status"
  [ "$MODE" = "dry-run" ] && break

  items_before=$(count_items)
  started=$(date '+%F %T'); t0=$(date +%s)
  out="$STATE/rounds/$(date +%F-%H%M).json"
  prompt="Run the run-backlog-discovery skill in night mode: this is an unattended run and nobody will answer a question. Launched by night-discovery.sh at $started with the $SCOPE weekly figure at $weekly% and tonight's ceiling at $ceiling%."
  (cd "$REPO" && claude -p "$prompt" --model "$MODEL" --output-format json < /dev/null > "$out" 2>> "$LOG" 9>&-)
  code=$?
  launched=$(( launched + 1 ))
  spent=$(( ($(date +%s) - t0) / 60 ))
  new_items=$(( $(count_items) - items_before ))

  # A meter that reset during the round cannot be subtracted; record no cost.
  w_cost=""; s_cost=""
  if after=$(read_usage); then
    read -r weekly_after session_after _ <<< "$after"
    [ "$weekly_after" -ge "$weekly" ] && w_cost=$(( weekly_after - weekly ))
    [ "$session_after" -ge "$session" ] && s_cost=$(( session_after - session ))
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$started" "$spent" "$weekly" "${weekly_after:-}" "$session" "${session_after:-}" "$new_items" "$code" "$w_cost" "$s_cost" >> "$COSTS"
  log "round ended: exit $code, $spent min, $new_items new item(s), weekly $weekly% → ${weekly_after:-?}%, output $out"

  if [ "$code" -ne 0 ]; then
    log "the round failed; no further launch tonight"
    break
  fi

  # Dry-stop: count consecutive rounds that recorded no new item.
  if [ "$new_items" -gt 0 ]; then
    echo 0 > "$STATE/dry-streak"
  else
    streak=$(( $(cat "$STATE/dry-streak" 2>/dev/null || echo 0) + 1 ))
    echo "$streak" > "$STATE/dry-streak"
    if [ "$streak" -ge "$DRY_STOP_AFTER" ]; then
      echo "$streak rounds with no new item, last on $(date +%F)" > "$STATE/paused"
      log "dry-stop: $streak consecutive rounds with no new item; paused"
      break
    fi
  fi

  [ "$MODE" = "trial" ] && break
done

log "done ($MODE): $launched round(s) launched"
exit 0
