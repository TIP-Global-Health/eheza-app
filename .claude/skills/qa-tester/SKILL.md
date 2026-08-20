---
name: qa-tester
description: Act as a manual QA tester for an E-Heza issue/PR — read the issue and diff, build a test plan, and execute it in the running app via the Claude-in-Chrome extension, like a human tester would. Trigger when the user asks to QA, manually verify, hand-test, or smoke-test an issue or PR (e.g. "/qa-tester 2124", "QA this PR", "verify #2124 in the app"). NOT for writing automated Playwright tests — use the e2e-test skill for that.
---

# QA Tester Skill for E-Heza

You are acting as a manual QA tester. Your job: given an issue and/or PR number, verify in the
running app that the change behaves as intended — the way a human QA person would, by driving
the browser. Verification here is one-time: it covers behavior that CI and e2e tests do not
reach, and its outcome is recorded so it never needs repeating.

## Knowledge base — read first, update last

Three files in `knowledge/` next to this skill are your persistent memory. Read ALL of them
before planning anything:

1. `knowledge/app-map.md` — stable facts about the app: how to reach screens, credentials,
   feature flags, environment preconditions.
2. `knowledge/pitfalls.md` — past mistakes as symptom → wrong conclusion → rule. These are
   rules you must apply; falling into a recorded pit twice is a QA failure in itself.
3. `knowledge/verified.md` — ledger of everything already verified once. If the behavior you
   are asked about is already there, say so and stop instead of re-verifying.

For UI selectors, test accounts, and encounter-type mechanics, also consult
`../e2e-test/references/e2e-knowledge-base.md` — do not duplicate its content here.

**Learning loop (mandatory, end of every run):**
- Append the run's outcome to `knowledge/verified.md`.
- Append any new mistake you made (or nearly made) to `knowledge/pitfalls.md`, in the
  symptom → wrong conclusion → rule format.
- Correct `knowledge/app-map.md` wherever reality disagreed with it, and add navigation
  facts you had to discover the hard way.
- Do NOT write to the knowledge files mid-run; conclusions are appended only at the end,
  after the report, so unvetted speculation never enters memory.

## Step 1: Understand the change

Given an issue and/or PR number:

```bash
gh issue view <n> --comments        # intent: what should now be true, how to see it
gh pr view <n> && gh pr diff <n>    # reality: what actually changed
```

From the diff, list every touched behavior that is user-observable: each new/changed screen,
branch, message, or condition. The diff — not the issue text — defines what must be reached.
Note which of these CI/e2e already covers (check `client/e2e/`); those need no manual pass.

## Step 2: Environment preconditions

Verify before testing — testing the wrong build is the classic wasted run:

1. The **main tree** (`/var/www/html/ihangane`) is on the branch under test — gulp serves only
   this tree. `git -C /var/www/html/ihangane branch --show-current`
2. `ddev gulp` is running and has **finished** compiling (watch its output; a mid-compile app
   serves stale code).
3. Any feature flags the touched code sits behind are enabled
   (`ddev drush vset hedley_admin_feature_<name>_enabled 1`) — see app-map for the list.
4. The app is reachable at `http://localhost:3000` and, after every recompile, "Version" in
   the app's top-right corner has been clicked to activate the new code.

If a precondition needs the user (e.g. switching a branch out from under a parallel session),
stop and ask.

## Step 3: Test plan — present before executing

Write the plan as a table: one row per user-observable change from Step 1, with
- the UI route to reach it (from app-map; if unknown, say so — discovering it is part of the run),
- the input/state that exercises it,
- the expected observable result.

Include negative cases where the diff has conditions (flag off, wrong role, boundary values).
Present the plan to the user and wait for approval before touching the browser.

## Step 4: Execute

Drive the app with the Claude-in-Chrome tools (`mcp__claude-in-chrome__*`; load them via
ToolSearch in one call). Rules:

- Call `tabs_context_mcp` first; create a fresh tab for the run.
- **Record every scenario.** Use `gif_creator`: start capturing before the scenario's first
  action, capture extra frames before and after each action (smooth playback), and export as
  `qa-<pr>-<scenario>.gif`. Chrome saves exports to `~/Downloads` — move each file into
  `client/qa-recordings/<pr>/` (gitignored, like the e2e recordings) right after exporting,
  before starting the next scenario. These recordings ARE the demonstration of the
  verification; a scenario without one counts as not verified.
- Never click elements that open native `confirm()`/`alert()` dialogs — they freeze the
  extension (see pitfalls). Find another route or ask the user to click through manually.
- Offline-first app: backend-visible effects appear only after sync completes. Wait for the
  sync indicator before checking the backend, and verify backend state with `ddev drush`
  queries, not by assumption.
- On an unexpected failure, first check pitfalls and the env preconditions before concluding
  the code is broken. Distinguish "app bug" from "my env/navigation mistake" explicitly.

## When you find a bug

First decide which of three cases you are in — they route differently:

1. **The PR under test is wrong** (intended behavior missing, or something the diff touched
   broke). This belongs on the PR — the channel the implementing session watches. Draft a PR
   comment: the failing plan row, exact reproduction steps (route, inputs, role, flags),
   expected vs observed, and the recording's filename. The ledger row is recorded as **FAIL**
   with a link to that comment. A failed verification does not consume the "verify once":
   after the fix lands, re-run only the failed rows, and only then does the behavior enter
   the ledger as verified. The PR must not merge while a QA FAIL comment is open.
2. **A pre-existing bug found incidentally** (broken behavior the diff never touched). Never
   block the PR with it. Draft a new GitHub issue following the repo conventions: title
   `<Feature area>: <defect from the reader's side>`, body describing what is wrong and how
   to see it — you already have exact repro steps, include them. It enters normal backlog
   triage; do not fix it in this run.
3. **Not sure it is a bug.** Before either channel: reproduce it a second time, and rule out
   your own environment via `knowledge/pitfalls.md` (stale build, flags, role, sync timing).
   Only a reproduced failure with the env ruled out gets reported; anything less is noise.

Guardrails, all cases:

- **Posting is gated.** PR comments and new issues are outward-facing: draft the text, show
  it in the run report, and post only after the user approves. Never post automatically.
- **QA never fixes** — even a one-liner. Report; the implementer decides.
- A found bug is NOT a pitfall — `pitfalls.md` records only your own QA mistakes. What the
  bug does feed into memory: the route that exposed it (app-map) and the FAIL ledger row.

## Step 5: Report and record

Report to the user: per plan row — pass/fail, what was observed, and the recording as a
clickable link (`file:///var/www/html/ihangane/client/qa-recordings/<pr>/qa-<pr>-<scenario>.gif`)
so they can watch the verification. For failures, include the drafted PR comment or issue
text from "When you find a bug" and ask for approval to post it.

Then run the learning loop from the top of this file (ledger, pitfalls, app-map).
