---
name: qa-tester
description: Act as a manual QA tester for an E-Heza issue/PR — verify in the running app that the change behaves as intended, the way an experienced human tester would, and record the evidence. Trigger when the user asks to QA, manually verify, hand-test, or smoke-test an issue or PR (e.g. "/qa-tester 2124", "QA this PR", "verify #2124 in the app"), or to watch for PRs labelled Ready for QA ("/loop /qa-tester watch"). NOT for writing automated Playwright tests — use the e2e-test skill for that.
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

**A run does not stop to ask.** Given a PR number, carry on until the report and recordings
are posted. Stop only when blocked, or before posting a FAIL comment or an issue. A watch run
stops for neither (see Watch mode).

## Watch mode — `/loop /qa-tester watch`

The user starts the watch in a session kept open for QA, and again after a reboot. Each PR
moves through labels: `Ready for QA` (set by the developer) → `QA Running` → one result.
The loop session only dispatches. Each PR is run by a fresh subagent, so the loop's context
stays small and no PR's run carries over into the next.

The loop paces itself, and each tick does this:

1. If a QA subagent this session started is still running (`ListAgents`), the tick ends:
   wake again in 1800 s, `noop: true`. Its completion notice wakes the loop sooner.
2. `bash .claude/skills/qa-tester/scripts/qa-watch.sh next`. If it prints nothing, the tick
   ends: `ScheduleWakeup` in 1200 s, `noop: true`, the same prompt.
3. `qa-watch.sh ready`, which first fast-forwards the main tree's `develop` so the run
   uses this skill's latest version. Anything but `ready` is a station problem, not the
   PR's: tell the user what is missing, change no label, and wake again in 1200 s with
   `noop: false`.
   Never start or restart ddev or gulp yourself.
4. `qa-watch.sh label <pr> running`, which also removes `Ready for QA`. Then start one
   `general-purpose` subagent with this prompt, and wake again in 1800 s:

   > Run the qa-tester skill (Skill tool, `qa-tester`, args `<pr>`) in watch mode, from
   > Step 1 to the learning loop, without stopping to ask. Do not change the PR's labels.
   > Start your reply with one line, `RESULT: approved|followup|blocked|ready — <why>`,
   > then give the report and recordings links, any drafted issue, and any helper proposal.

5. When the subagent finishes, set the label from its `RESULT` line with
   `qa-watch.sh label <pr> <state>`, and relay its links, drafted issue and helper proposal
   to the user. With no `RESULT` line, leave `QA Running`, so the next tick runs it again.
   Then wake again in 60 s, so a second labelled PR does not wait.

The subagent picks the result:

- `approved` — every row passed.
- `followup` — a row failed because the PR is wrong (case 1 under "When you find a bug").
  The FAIL comment is posted without waiting for approval.
- `blocked` — the PR could not be tested: it does not build, a screen cannot be reached, or
  its data cannot be set up. The report says what blocked it. Any blocked row makes the
  run blocked, unless another row failed, which makes it `followup`.
- `ready` — the run stopped on a station problem, such as local changes to the PR's files
  in Step 2. Nothing is posted; the PR goes back in the queue.

`next` lists a PR labelled `QA Running` first. When no subagent of this session is running
it, its run was cut short, for example when a session closed. Run it again from the start;
if that run already posted a report, the new run replaces it with `--update`.

A watch run never posts a new issue for a pre-existing bug, and nobody is there to answer a
helper proposal: both go in the subagent's reply, for the loop to relay. Removing
`Approved by QA` after new commits land is the user's manual step.

## Knowledge base — read first, update last

Read ALL of these before planning:

1. `knowledge/app-map.md` — what helpers cannot hold: accounts and roles, which screen
   shows what, sync gates, environment facts, and route notes for screens no helper reaches.
2. `knowledge/pitfalls.md` — past QA mistakes as symptom → wrong conclusion → rule. Falling
   into a recorded pit twice is a QA failure in itself. Pitfalls explain a failure on one
   build. They never explain a difference between two builds run on the same fixture in
   the same session: the environment is held constant, so that difference is the change's
   doing until a second fixture says otherwise.
3. `knowledge/verified.md` — ledger of what is already verified. If the behaviour asked
   about is there, say so and stop.

Also list the helpers (`grep -n "^export" client/e2e/helpers/*.ts`), and use
`../e2e-test/references/e2e-knowledge-base.md` for selectors and per-encounter mechanics.

**Learning loop (mandatory, at the end of every run, after the report):**
- Append the outcome to `knowledge/verified.md`: one row per behaviour, then the run's
  summary row — how many fix, hold and boundary rows ran, and how many findings they made.
  After ten runs those rows say whether hold rows ever fail.
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
grep -lw "#<n>" .claude/backlog/items/*.md   # backlog item, if the work came from one
```

A matching backlog item adds context: the code paths, how the defect happens, related items,
and what was verified before merge. Read it when it exists; many PRs have none.

When the backlog item disagrees with the issue or the PR, the issue and the PR win. The item
records what was believed when it was written; they record what was asked and shipped.

List every user-observable behaviour the diff touches: each new or changed screen, branch,
message, or condition. The diff — not the issue text, not the commit subjects — defines
what must be reached (see pitfalls on reverted commits). Scope is what a tester does in
the app — the tablet and the admin UI. One-off data scripts (`server/**/scripts/*.php`),
migrations and update hooks are not tested here: leave them out of the plan and the report.

Then list what else the change can reach: every caller of each changed function
(`grep -rn '<name>\b' client/src/elm`), and every reader of data it writes (reports,
Case Management panes, dashboards, Statistical Queries). A caller the issue's scenario
does not run is a hold row in Step 3. For a changed function with three callers, the
issue usually drives one.

Not every change has callers. A diff that widens a condition — three activities become
every activity, one role becomes both — or moves a one-shot action into a fetch, a render
or a sync cycle, is covered by what it now includes. So the plan also names every non-test
file in the diff, with the row that exercises its change or one line saying why it needs
none: import-only, a signature change, a removed message. A file no row reaches and no
line excuses is a gap in the plan, not in the report.

When a condition is widened, each newly included member that renders through different
view code is a hold row. One screen stands in for another only when both render through
the same view function, and the report says which stood in for which.

Note what `client/e2e/` already covers; that needs no manual pass. The same holds for
criteria the backlog item records as fail-first unit tests. A tested criterion needs an
end-to-end row only once per user-visible path the diff touches, to prove the wiring:
that each caller passes the right data and the screen shows it. The report lists the
other criteria as covered by tests, by name.

## Step 2: Environment

1. The PR's client code is in the **main tree** (`/var/www/html/ihangane`) — gulp serves
   only that tree, and other sessions share it. Starting a run is the go-ahead to take it;
   stop only if the `git status` below shows changes you did not make. Do not
   check the PR branch out: a branch older than this skill swaps the skill's own files.
   Overlay only the files the PR changes, and unstage them, because the `Stop` hook commits
   whatever is staged:

   ```bash
   b=origin/<branch>; git fetch -q origin "${b#origin/}"
   files=$(gh pr diff <n> --name-only | grep '^client/')
   git status --short -- $files    # must print nothing: never overwrite local changes
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

## Step 3: Test plan — present, then execute

A table, one row per behaviour from Step 1, each written as a tester's instruction in plain
words: where, what to do, and what to verify — "At an NCD encounter, in the Social History
form, answer that the patient drinks and smokes, enter both counts, save, and verify that
the drinks and cigarettes counts are stored as entered." Beside it, for your own use, how to
reach it (the helper chain, or by hand with the route from app-map). Include negative
cases where the diff has conditions (flag off, wrong role). Present it, then go straight
on to Step 4 without waiting for approval. The user can interrupt to change it.

An expected result comes from one of four places: the issue's expected behaviour; `develop`
on the same fixture, for behaviour the PR must not change; a second screen, role or the
server showing the same data; or the exact value in a condition the diff adds. The view
code gives the route to a screen, never the expected value: a wrong implementation yields
a wrong expectation that the row then passes against. A row with no oracle outside the
code under test is a smoke row, labelled so, and is not counted as coverage.

Every plan has three kinds of row, and each row names its kind:

- **Fix rows**, from the issue: the defect on `develop`, gone with the PR. The `develop`
  control runs first, on the same fixture, one per fixture shape (see How to drive).
- **Hold rows**, from the blast radius: scenarios the change must not alter. Run each on
  both builds with the same data and compare the stored record and the screen. Any
  difference is a FAIL; no judgement about whether it looks right is needed or allowed.
  The row's write-up names `develop`'s value beside the PR's, or says "same as develop".
  A row reported from the PR side alone is a proof row, and is labelled so.
- **Boundary rows**: the exact value of each condition the diff adds or moves. Equal to
  the threshold, the same day, the day after, an empty collection, a missing value.

Hold rows are where regressions live. A diff of more than three non-test files needs at
least three, or the plan says what was looked for and why nothing qualifies.

When the diff changes what the app does rather than what it shows — work moved into a
fetch, a render or a sync cycle — a hold row's oracle is the request count. Every reply
carries `requests`: how many the command caused and the busiest paths. Run the same
action on both builds and compare. A tab switch that costs three requests on `develop`
and thirty on the PR is a FAIL the error channel cannot see.

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
`(x) Yes` / `( ) No`, and field values. `requests` is the number of requests the command
caused, with the busiest paths (ids replaced by `<id>`). While recording, `recordingAt` is
the second of the video the command ended at. Read that
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
- **One develop control per fixture shape, not per row.** A control proves a row reaches
  the defect; once it has for one row, rows that reuse that shape through another role or
  screen run on the PR only. A new threshold with a different setup gets its own control.
  A hold row's develop run is its oracle, so it always runs; only its stored record and
  screen are compared, and only the PR side is recorded.
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
- On an unexpected failure, check pitfalls and Step 2 before concluding the code is broken,
  and say explicitly whether it is an app bug or your own mistake.

### The error channel

Every reply lists `consoleErrors`: console errors, uncaught exceptions, failed requests and
responses of status 400 or more since the last command. Read it after every scenario, on
both builds. An error the PR's run shows and the `develop` run of the same scenario does
not is a FAIL. One both builds show is a case 2 issue, drafted with the scenario that
raised it. Never clear it unread. The report states what each build showed; "none on the
PR" alone is half a reading, since `develop` is the baseline it is compared with.

### Proving the data was saved

Straight after Save, a form shows the clicks, not what was stored. So every test that
saves data proves the server holds it, proof 1 below. Proof 2 is added only when the diff
touches what the device reads back after a save: decoders, IndexedDB, the local model
update. Proof 3 is added only when the diff touches sync, encoders, decoders or server
code. Each plan row names the proofs the diff called for, and the report repeats them.

1. **Uploaded.** Sync, then Device Status must show nothing left to upload. Only then
   query the backend, with drush or the helpers' `query*` functions:

   ```js
   await h.common.syncAndWait(page);
   await qa.click(page.locator('span.sync-icon'));
   const hc = page.locator('.health-center', { hasText: 'Nyange' }).locator('.sync-status');
   qa.check('all uploaded', /Remaining for Upload: 0\b/.test(await hc.innerText()), true);
   ```
2. **Loaded from the device.** Reload (`page.reload()`, signing in again if asked), reach
   the form through the UI, and check every saved value. Now the screen shows stored data.
3. **Downloaded.** Re-pair: `qa.sh stop`, `qa.sh start --fresh`, then `qa.freshDevice()`.
   The new device starts empty, so saved values on the form prove they went up and came down.

Re-pairing ends the browser session, so do it once, after the last test, and open every
test's saved form in one recording, `qa-<pr>-after-repair.mp4`. For an ended encounter,
open the screen that shows its data, such as the progress report.

### Recording

Record every test **from the moment its encounter starts** — or, outside encounters, from
the first screen of the flow under test — through to the result, helper-driven activities
included: `qa-<pr>-test<n>-<name>.mp4`. `<n>` is the row's number in the plan table, and
that number is frozen before the first recording: a row added later goes at the end, and
no row is renumbered. The report, the captions and the ledger all name a video by it.

**Preparation is recorded once per fixture shape, on the PR build.** Each encounter a test
needs beforehand — the initial encounter before a recurrent one, an earlier visit, an
illness the record should list — gets its own video from its start to its end,
`qa-<pr>-prep<k>-<name>.mp4`, so no video runs long. Two preparation encounters make two
videos. A control's preparation is the same encounter on `develop`, which the diff does not
change; it runs unrecorded. Only device setup,
registering the patient, sync and database steps such as backdating stay off video. So
each fixture stops on the participant page, and its recorded command starts the encounter. The video is real time, with the e2e cursor marking the pointer; while
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
   PR with it. First grep `.claude/backlog/items/` for the symptom's words: the field, the
   node type, the screen, the message. A matching item gets this run's example appended,
   with the recording's path, and the report names the item instead of a new issue.
   Otherwise draft a new issue following the repo conventions: title
   `<Feature area>: <defect from the reader's side>`, body saying what is wrong and how to
   see it, with your repro steps. Do not fix it in this run.
3. **Not sure it is a bug.** Reproduce it a second time and rule out your own environment
   via pitfalls (stale build, flags, role, sync timing). Only a reproduced failure with the
   environment ruled out gets reported.

A FAIL needs all three: reproduced on a second fresh fixture; an oracle from Step 3, not
the tester's preference; and for a hold row, a `develop` run that differs. Wording,
spacing, naming and file names are never a FAIL. They become a case 2 issue only when a
sibling screen does the same thing differently, the comparison being the oracle.

In all cases: a FAIL comment or a new issue is posted only after the user approves the
drafted text. In watch mode the FAIL comment is posted without approval; a new issue still
waits, drafted in the run's reply. QA never fixes — even a one-liner. A found bug is not a pitfall; it feeds
memory only as the route that exposed it (app-map) and the FAIL ledger row.

## Step 5: Report, post, and record

Report to the user per plan row — pass/fail, what was observed, and the recording as a
link (`file:///var/www/html/ihangane/client/qa-recordings/<pr>/qa-<pr>-<scenario>.mp4`).
For failures, include the drafted comment or issue and ask for approval to post it — in
watch mode, the posted FAIL comment's link and any drafted issue.

**Then post the QA report to the PR — every run, pass or fail, without waiting for
approval.** Its first line is exactly this, with no `>` or heading mark in front:

```
**Manual tests executed using the QA Tester skill**
```

then a table with one row per plan row — the plain-words test from Step 3, its kind (fix,
hold, boundary) and its result — and a short paragraph per row saying what it showed —
including which part was set up by helpers, which was checked by hand, and which proofs
the diff called for. Then one sentence on what the error channel showed on each build.
Name the criteria covered by unit tests. Close with what the run did
**not** cover:
blocked rows, touch-only behaviour, and sibling code paths a scenario stands in for but did
not exercise. State a blocked row as blocked; never imply coverage the run does not have.
Describe what is now true, not which commands were run.

```bash
bash .claude/skills/qa-tester/scripts/post-qa-report.sh <pr> <drafted-file>
```

It refuses a file that does not open with the caption, or that has no error-channel
sentence naming both builds; `--update <comment-id>` replaces an earlier report instead
of adding one. The report names no video files.

**Then post the recordings as a separate comment**, one player per video. Preparation comes
first, each as `**Preparation <k>:** <what is prepared, and which test needs it>`, then each
test as `**Recording — test <n>:** <one line on what it shows>`, and last
`**After re-pairing:** <the saved values each test's form shows>`, each with its video below.
Attach the files; `gh` uploads them and puts each player where its reference is:

```bash
# recordings.md:  **Recording — test 1:** …  then a line  ![](client/qa-recordings/<pr>/qa-<pr>-<name>.mp4)
gh pr comment <pr> --body-file recordings.md --attach client/qa-recordings/<pr>/qa-<pr>-<name>.mp4
```

A video may be at most 10 MB on a Free plan. Check the posted comment renders a `<video>`.

Then stop the driver and run the learning loop. In watch mode, the reply opens with the `RESULT`
line, and the loop sets the label.

## Fallback: Claude in Chrome

If the driver cannot run, the run can still be done through the Claude-in-Chrome extension,
at a much higher cost per step. `knowledge/chrome-extension.md` holds what that needs.
