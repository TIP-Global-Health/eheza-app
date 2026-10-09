#!/bin/bash
# Turn a gif_creator export into the kept .mp4 recording, check it, and delete the GIF.
#
#   bash .claude/skills/qa-tester/scripts/save-recording.sh <pr> <scenario> [seconds]
#
# Reads ~/Downloads/qa-<pr>-<scenario>.gif (DOWNLOADS overrides the folder) and
# writes client/qa-recordings/<pr>/qa-<pr>-<scenario>.mp4 in the main tree.
# With [seconds], also saves the frame at that time as a PNG to look at.

set -eu

pr=${1:?usage: $0 <pr> <scenario> [seconds]}
scenario=${2:?usage: $0 <pr> <scenario> [seconds]}
at=${3:-}
name="qa-$pr-$scenario"

downloads=${DOWNLOADS:-$HOME/Downloads}
gif="$downloads/$name.gif"
if [ ! -f "$gif" ]; then
  # Chrome renames a repeated download to "name (1).gif"; take the newest.
  gif=$(ls -t "$downloads/$name"*.gif 2>/dev/null | head -1 || true)
fi
[ -n "$gif" ] && [ -f "$gif" ] || { echo "no export found: $downloads/$name.gif" >&2; exit 1; }

main=$(dirname "$(git -C "$(dirname "$0")" rev-parse --path-format=absolute --git-common-dir)")
outdir="$main/client/qa-recordings/$pr"
mp4="$outdir/$name.mp4"
mkdir -p "$outdir"

# The scale filter is required: captures can have an odd height, and yuv420p needs even sides.
ffmpeg -y -loglevel error -i "$gif" \
  -movflags +faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" \
  -c:v libx264 -crf 23 "$mp4"

errors=$(ffmpeg -v error -i "$mp4" -f null - 2>&1)
if [ -n "$errors" ]; then
  echo "decode check failed, GIF kept at $gif:" >&2
  echo "$errors" >&2
  exit 1
fi
rm "$gif"

duration=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$mp4")
echo "saved: $mp4 (${duration}s of playback, not real time)"

if [ -n "$at" ]; then
  frame="$(mktemp -d)/$name-at-$at.png"
  ffmpeg -y -loglevel error -ss "$at" -i "$mp4" -frames:v 1 "$frame"
  echo "frame: $frame"
fi
