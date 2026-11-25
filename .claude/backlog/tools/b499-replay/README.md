# B-499 replay harness

Replays build `7ece1b2d0`'s Acute Illness and Prenatal diagnosis logic over encounters rendered from live. Used for B-499's checks.

1. Collect the node ids with read-only SQL: the encounters, their participants, persons, measurements and nurses (queries in B-499's entry, 2026-11-25).
2. Put the ids in `render.php.tmpl` and run it once with `terminus remote:drush ihangane.live -- php-eval "$(cat render.php)" > payload.json`. It only reads.
3. Make a worktree at `7ece1b2d0`, set `elm-version` to 0.19.3 in `client/elm.json`, copy `Test.elm` to `client/src/elm/B499Replay/`.
4. Generate `B499Replay/Payload.elm` from the payload's `batch` as one Elm string (escape `\`, `"` and control characters).
5. Run `elm-test src/elm/B499Replay/Test.elm`. The report lists, per encounter: type, nid, the build's diagnosis, the stored one, the gate.

⛔ The payload holds personal data and nurse PINs. Keep it in the scratchpad; never commit it.
