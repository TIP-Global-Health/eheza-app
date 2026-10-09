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

**Your hands are the e2e helpers.** `client/e2e/helpers/` holds about 220 functions that
drive every encounter type through the real UI — `h.prenatal.completeDangerSigns(page)`,
`h.auth.setupDevice(page)`, `h.common.registerAdult(...)`. The QA driver (below) runs them in
a visible browser, so a known path costs one command, not one tool call per click.

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
gh issue view <n> --json title,body,comments    # intent: what should now be true
gh pr view <n> --json title,body,commits,files  # plain `gh issue/pr view` fails here
gh pr diff <n>                                  # reality: what actually changed
```

List every user-observable behaviour the diff touches: each new or changed screen, branch,
message, or condition. The diff — not the issue text, not the commit subjects — defines
what must be reached (see pitfalls on reverted commits). Note what `client/e2e/` already
covers; that needs no manual pass.

## Step 2: Environment

1. The **main tree** (`/var/www/html/ihangane`) is on the branch under test — gulp serves
   only that tree. If switching it would disturb a parallel session, stop and ask.
2. `ddev gulp` has **finished** compiling.
3. Feature flags the touched code sits behind are on (see app-map).
4. Start the driver on a fresh profile and set up a fresh device in one command:

   ```bash
   Q=.claude/skills/qa-tester/scripts/qa.sh
   bash $Q start --fresh
   echo "await qa.freshDevice();" | bash $Q run
   ```

   That pairs a new device, signs in nurse Maya at Nyange and syncs the health centre —
   about 10 seconds. `qa.freshDevice({ pin: '2345', location: '<village>' })` does the
   same for another account; to switch accounts later, see app-map.
5. The build in the driver's browser is the one under test: `span.version` in the top
   right reads `Version: <hash>`, which must equal `git rev-parse --short HEAD` in the main
   tree. After a recompile, click it and apply the update on the page it opens.

## Step 3: Test plan — present before executing

A table, one row per behaviour from Step 1: how to reach it (the helper chain, or "by hand"
with the route from app-map), the input or state that exercises it, and the expected
result. Include negative cases where the diff has conditions (flag off, wrong role,
boundary values). Present it and wait for approval.

## Step 4: Execute

### The driver

| command | does |
|---|---|
| `qa.sh start [--fresh]` | open the browser; it stays open between commands and reopens the last page |
| `qa.sh run [file] [--shot]` | run JS (file or stdin) as the body of `async (page, h, qa) => {...}` |
| `qa.sh state [--shot]` | summary of the screen; `--shot` also saves a PNG to look at |
| `qa.sh record start <pr> <name>` / `record stop` | real-time video to `client/qa-recordings/<pr>/qa-<pr>-<name>.mp4` |
| `qa.sh stop` | close the browser (kills it if a command is stuck) |

Every reply is JSON: the command's return value or error, its duration, and the screen it
left — URL, headings, open dialogs, task counters, tabs, buttons with `[active]` /
`[completed]` / `[disabled]`, choices as `(x) Yes` / `( ) No`, and field values. Read that
instead of taking a screenshot to find out where you are. In the command body, `page` is
the Playwright page, `h` holds every helper module (`h.prenatal`, `h.common`, `h.ncd`, …),
and `qa` has `state()`, `shot(name)`, `log(...)`, `click(locator | text)` and `freshDevice()`.

### How to drive

- **Known path: one command.** Chain helpers for all the setup the scenario needs, e.g.
  register, start the encounter, complete the activities before the one under test. Read
  a helper's signature and options before calling it.
- **The screen under test: small steps, and look.** One action or a few per command, read
  the reply, and `--shot` at least once in every state that matters — content, behaviour
  *and* appearance (see pitfalls on judging a new dialog by its text).
- **Act as a user acts.** Click with `qa.click` or helpers, which hover first like a person;
  type with `locator.fill`; choose with `selectOption`. Reach the screen under test through
  the UI — never `page.goto('#...')` to it. A Playwright click refuses an element a person
  could not press (hidden, covered, disabled), so a click that goes through is honest.
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

Record every scenario: `record start` before its first action, `record stop` after the
result is on screen. The video is real time, with the e2e cursor marking the pointer; while
recording, every helper click hovers and pauses for a second so the viewer can follow.
Setup that is not evidence runs unrecorded at full speed. Look at a frame from the moment
that matters (`ffmpeg -ss <t> -i <video> -frames:v 1 frame.png`) — a video that does not
legibly show the thing demonstrated is not evidence. A scenario without one is not verified.

The driver runs the e2e device: iPad Mini viewport, mouse input. Touch-only gestures are not
exercised; say so in the report when the change depends on them.

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

then the build and login used, a table with one row per plan row and its result, and a
short paragraph per row saying what it showed. Close with what the run did **not** cover:
blocked rows, touch-only behaviour, and sibling code paths a scenario stands in for but did
not exercise. State a blocked row as blocked; never imply coverage the run does not have.
Describe what is now true, not which commands were run.

```bash
bash .claude/skills/qa-tester/scripts/post-qa-report.sh <pr> <drafted-file>
```

It posts through the REST API (`gh pr comment` fails on this repo) and refuses a file
that does not open with the caption. Then stop the driver and run the learning loop.

## Fallback: Claude in Chrome

If the driver cannot run, the run can still be done through the Claude-in-Chrome extension,
at a much higher cost per step. `knowledge/chrome-extension.md` holds what that needs.
