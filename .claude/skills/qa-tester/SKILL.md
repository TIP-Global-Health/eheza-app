---
name: qa-tester
description: Act as a manual QA tester for an E-Heza issue/PR — verify in the running app that the change behaves as intended, the way an experienced human tester would, and record the evidence. Trigger when the user asks to QA, manually verify, hand-test, or smoke-test an issue or PR (e.g. "/qa-tester 2124", "QA this PR", "verify #2124 in the app"). NOT for writing automated Playwright tests — use the e2e-test skill for that.
---

# QA Tester Skill for E-Heza

You replace a human QA tester who knows the app very well. That tester does not explore
menus or wonder what a form wants: they know what to fill and what to press, move through
familiar screens fast, and slow down only on the screen the change touches, where they look
closely and try the edge cases. Verification is one-time: it covers what CI and e2e tests do
not reach, and its outcome is recorded so it never needs repeating.

**Your hands are the e2e helpers.** `client/e2e/helpers/` holds about 280 functions that
drive every encounter type through the real UI — `h.prenatal.completeDangerSigns(page)`,
`h.common.registerAdult(...)`, `h.common.openActivity(...)`. The QA driver (below) runs them
in a browser it keeps open, so a known path costs one command, not one tool call per click.

## Knowledge base — read first, update last

Read ALL of these before planning:

1. `knowledge/app-map.md` — what helpers cannot hold: accounts and roles, which screen
   shows what, sync gates, environment facts, and route notes for screens no helper reaches.
2. `knowledge/pitfalls.md` — past QA mistakes as symptom → wrong conclusion → rule. Falling
   into a recorded pit twice is a QA failure in itself.
3. `knowledge/verified.md` — ledger of what is already verified. If the behaviour asked
   about is there, say so and stop.

Also list the helpers (`grep -n "^export" client/e2e/helpers/*.ts`), and use
`../e2e-test/references/e2e-knowledge-base.md` for selectors and per-encounter mechanics.

**Learning loop (mandatory, at the end of every run, after the report):**
- Append the outcome to `knowledge/verified.md`.
- Append any mistake you made (or nearly made) to `knowledge/pitfalls.md`.
- Correct `knowledge/app-map.md` where reality disagreed, merging into the section the
  fact belongs to — never append a new dated section.
- If you drove a form by hand that a later run will need again, propose it to the user as
  a new e2e helper. Helpers are product code, so this is their call and goes through a PR.

Do not write to the knowledge files mid-run; unvetted guesses never enter memory.

## Step 1: Understand the change

```bash
gh issue view <n> --comments    # intent: what should now be true
gh pr view <n>                  # needs gh 2.99 or later, as does --attach below
gh pr diff <n>                  # reality: what actually changed
```

List every user-observable behaviour the diff touches: each new or changed screen, branch,
message, or condition. The diff — not the issue text, not the commit subjects — defines
what must be reached (see pitfalls on reverted commits). Note what `client/e2e/` already
covers; that needs no manual pass. Scope is what a tester does in the app — the tablet and
the admin UI. One-off data scripts (`server/**/scripts/*.php`), migrations and update hooks
are not tested here: leave them out of the plan and the report.

## Step 2: Environment

1. The PR's client code is in the **main tree** (`/var/www/html/ihangane`) — gulp serves
   only that tree, and other sessions share it, so ask the user before taking it. Do not
   check the PR branch out: a branch older than this skill swaps the skill's own files.
   Overlay only the files the PR changes, and unstage them, because the `Stop` hook commits
   whatever is staged:

   ```bash
   b=origin/<branch>; git fetch -q origin "${b#origin/}"
   files=$(git diff --name-only "$(git merge-base develop $b)" $b -- client)
   git checkout $b -- $files && git reset -q -- $files
   ```

   When the run is over, `git checkout HEAD -- $files` gives `develop` back.
2. `ddev gulp` is running and has finished compiling — its output is in the user's
   terminal; if the build check in 5 fails, ask them rather than touching ddev.
3. Feature flags the touched code sits behind are on (see app-map).
4. Start the driver on a fresh profile and set up a fresh device in one command:

   ```bash
   Q=.claude/skills/qa-tester/scripts/qa.sh
   bash $Q start --fresh
   echo "await qa.freshDevice();" | bash $Q run
   ```

   That pairs a new device, signs in nurse Maya at Nyange and syncs the health centre —
   about 10 seconds. It refuses a used profile, where the app would keep its old pairing.
   To change account later: `await qa.signIn('<pin>', '<location text>')`. A driver that
   is already running can be reused: `qa.sh state` shows who is signed in (on the main
   menu) and the build.
5. The build in the driver's browser is the one under test. With a PR overlaid, the
   `version` label still names `develop`'s commit, so check by content: a name the PR adds
   appears in `client/serve/Main.js` once gulp has rebuilt (its modified time changes), and
   in the bundle the browser loaded. Without an overlay, `git diff --quiet <version> --
   client/src` must exit 0; comparing with HEAD gives false alarms. A fresh profile loads
   the new build; a used one needs `div.version-env` clicked and the update applied.

## Step 3: Test plan — present before executing

A table, one row per behaviour from Step 1, each written as a tester's instruction in plain
words: where, what to do, and what to verify — "At an NCD encounter, in the Social History
form, answer that the patient drinks and smokes, enter both counts, save, and verify that
the drinks and cigarettes counts are stored as entered." Beside it, for your own use, how to
reach it (the helper chain, or by hand with the route from app-map). Expected results come from the diff and the Elm view code that renders the screen —
reading that code is part of planning, not a shortcut. Include negative cases where the diff has conditions (flag off, wrong role,
boundary values). Present it and wait for approval.

## Step 4: Execute

### The driver

| command | does |
|---|---|
| `qa.sh start [--fresh] [--watch]` | open the browser; it stays open between commands and reopens the last page. `--watch` shows the window, which a screen shorter than 1024 px crops — videos too |
| `qa.sh run [file] [--shot]` | run JS (file or stdin) as the body of `async (page, h, qa) => {...}` |
| `qa.sh state [--shot]` | summary of the screen; `--shot` also saves a PNG to look at |
| `qa.sh record start <pr> <name>` / `record stop` | real-time video to `client/qa-recordings/<pr>/qa-<pr>-<name>.mp4` |
| `qa.sh frame <pr> <name> <seconds>` | save that video's frame at `<seconds>` as a PNG to look at |
| `qa.sh stop` | close the browser (kills it if a command is stuck) |

Every reply is JSON: the command's return value or error, its duration, and the screen it
left — URL, build, who is signed in (main menu only), headings, open dialogs, task
counters, tabs, buttons with `[active]` / `[completed]` / `[disabled]`, choices as
`(x) Yes` / `( ) No`, and field values. While recording, `recordingAt` is the second of the
video the command ended at. Read that
instead of taking a screenshot to find out where you are. In the command body, `page` is
the Playwright page, `h` holds every helper module (`h.prenatal`, `h.common`, `h.ncd`, …),
and `qa` has `state()`, `shot(name, { fullPage })`, `log(...)`, `click(locator | text)`,
`fill(locator, value)`, `form()`, `check(label, actual, expected)`, `signIn(pin, location)`
and `freshDevice()`. `qa.form()` reads the open form: each yes/no question's answer (`null`
when unanswered), select values, the task counter, and whether Save is on.

### How to drive

- **Known path: one command.** Chain helpers for all the setup the scenario needs, e.g.
  register, start the encounter, complete the activities before the one under test. Read
  a helper's signature and options before calling it. To stop on a form instead of
  completing it: `h.common.openActivity(page, '<encounter>', '<activity icon>')` and
  `h.common.clickSubTaskTab(page, '<tab icon>')`, with the names taken from the helpers.
- **The screen under test: one scripted command per scenario.** The code says which
  questions appear and when, so write the whole scenario before running it: each action,
  then `qa.check` on what the screen must show next (`qa.form()` and `qa.state()` give the
  values), and `qa.shot` in every state that matters. A mismatch stops the scenario with a
  screenshot — the only time to pause and look. Start and stop the recording in the same
  shell call as the run, so the video holds no thinking time (about 8 s instead of 53 s
  for the same malaria scenario).
- **Then review like a tester.** After the run, look at every screenshot it took —
  content, behaviour *and* appearance (see pitfalls on judging a new dialog by its text).
  Checks prove what the code promised; only looking catches what nobody wrote down.
- **Act as a user acts on the screen under test.** Click with `qa.click`, type with
  `qa.fill`, choose with `selectOption`. Both hover first, so the video shows where each
  action lands, and a Playwright click refuses an element a person could not press (hidden,
  covered, disabled). Many helpers click directly, some with `force: true`, which skips that
  check — fine for setup, not for the behaviour being verified. Reach the screen under test
  through the UI — never `page.goto('#...')` to it.
- **A failure names its locator after 10 seconds.** Read the reply's state and the
  screenshot it carries, then decide; never retry blindly.
- **Native dialogs are dismissed automatically** (= Cancel). To accept one, register
  `page.once('dialog', d => d.accept())` in the same command, before the click.
- **Photos**: `setInputFiles` on the Dropzone's hidden file input (as the photo helpers do).
- Backend effects land only after sync. `h.common.syncAndWait(page)` waits for it; then
  check with drush or the helpers' `query*` functions.
- On an unexpected failure, check pitfalls and Step 2 before concluding the code is broken,
  and say explicitly whether it is an app bug or your own mistake.

### Recording

Record every scenario **from the moment the encounter starts** — or, outside encounters,
from the first screen of the flow under test — through to the result. Everything inside
that span is on the video, helper-driven activities included. Encounters that create the
data a test then looks at — an earlier visit, an illness the record should list — are part
of that story: record them too, from their start, so the viewer sees where the data came
from. Only device setup, registering the patient, sync and database steps such as
backdating stay off the video. So a fixture stops on the participant page, and the
recorded command starts the encounter. The video is real time, with the e2e cursor marking the pointer; while
recording, `qa.click`, `qa.fill` and helper clicks made through `click()` hover and pause
for a second so the viewer can follow. Setup that is not evidence runs unrecorded at full
speed. Look at the frame from the moment that matters (`qa.sh frame`, at the `recordingAt`
of that command) — a video that does not legibly show the thing demonstrated is not
evidence. A scenario without one is not verified.

The driver runs the device the e2e recordings use: an 820×1024 tablet viewport, mouse input. Touch-only gestures are not
exercised; say so in the report when the change depends on them. Persons and encounters a
run creates stay in the local database, as e2e's do; nothing needs cleaning up.

## When you find a bug

First decide which case you are in:

1. **The PR under test is wrong** (intended behaviour missing, or something the diff touched
   broke). Draft a PR comment: the failing plan row, exact steps (route, inputs, role,
   flags), expected vs observed, and the recording's filename. The ledger row is **FAIL**
   with a link to that comment. After the fix lands, re-run only the failed rows; only then
   does the behaviour enter the ledger as verified. The PR must not merge while a QA FAIL
   comment is open.
2. **A pre-existing bug found incidentally** (the diff never touched it). Never block the
   PR with it. Draft a new issue following the repo conventions: title
   `<Feature area>: <defect from the reader's side>`, body saying what is wrong and how to
   see it, with your repro steps. Do not fix it in this run.
3. **Not sure it is a bug.** Reproduce it a second time and rule out your own environment
   via pitfalls (stale build, flags, role, sync timing). Only a reproduced failure with the
   environment ruled out gets reported.

In all cases: a FAIL comment or a new issue is posted only after the user approves the
drafted text. QA never fixes — even a one-liner. A found bug is not a pitfall; it feeds
memory only as the route that exposed it (app-map) and the FAIL ledger row.

## Step 5: Report, post, and record

Report to the user per plan row — pass/fail, what was observed, and the recording as a
link (`file:///var/www/html/ihangane/client/qa-recordings/<pr>/qa-<pr>-<scenario>.mp4`).
For failures, include the drafted comment or issue and ask for approval to post it.

**Then post the QA report to the PR — every run, pass or fail, without waiting for
approval.** It opens with the caption

> **Manual tests executed using the QA Tester skill**

then a table with one row per plan row — the plain-words test from Step 3 and its result —
and a short paragraph per row saying what it showed — including which part was set up by helpers
and which was checked by hand. Close with what the run did **not** cover:
blocked rows, touch-only behaviour, and sibling code paths a scenario stands in for but did
not exercise. State a blocked row as blocked; never imply coverage the run does not have.
Describe what is now true, not which commands were run.

```bash
bash .claude/skills/qa-tester/scripts/post-qa-report.sh <pr> <drafted-file>
```

It refuses a file that does not open with the caption; `--update <comment-id>` replaces
an earlier report instead of adding one. The report names no video files.

**Then post the recordings as a separate comment**, one player per test. Write each as
`**Recording — test <n>:** <one line on what it shows>` with the video referenced below it,
and attach the files; `gh` uploads them and puts each player where its reference is:

```bash
# recordings.md:  **Recording — test 1:** …  then a line  ![](client/qa-recordings/<pr>/qa-<pr>-<name>.mp4)
gh pr comment <pr> --body-file recordings.md --attach client/qa-recordings/<pr>/qa-<pr>-<name>.mp4
```

A video may be at most 10 MB on a Free plan. Check the posted comment renders a `<video>`.

Then stop the driver and run the learning loop.

## Fallback: Claude in Chrome

If the driver cannot run, the run can still be done through the Claude-in-Chrome extension,
at a much higher cost per step. `knowledge/chrome-extension.md` holds what that needs.
