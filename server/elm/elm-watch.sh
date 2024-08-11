#!/usr/bin/env bash

# nice colors
COLOR_OFF="\e[0m";
DIM="\e[2m";

# random filename for the lock; see below
LOCKNAME=$(cat /dev/urandom | tr -cd 'a-f0-9' | head -c 16);

function run {
  (
  flock 200; # don't let multiple `elm make` scripts run at once.

  # reset the terminal scrollback history
  # --> all the errors you see are the current ones, not stale
  clear;
  tput reset;

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  elm make src/Main.elm --output ../hedley/modules/custom/hedley_general/js/elm-main.js
  # on Linux optionally prepend for better performance: sysconfcpus -n 1

  ) 200>"/var/lock/${LOCKNAME}"
}

# run the compiler when running the script...
run;

# ... and when you save files in these directories
inotifywait -mqr -e close_write --format '%w %e %f' ./src | while read DIR EVENT FILE; do
  run;
done;
