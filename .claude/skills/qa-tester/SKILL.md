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

5. **The Chrome window is visible and in front.** Check it, first thing, with
   `javascript_tool: JSON.stringify({hidden: document.hidden, focus: document.hasFocus()})`.
   If `hidden` is true, ask the user to raise the window before going on — this is not a
   nicety. Hidden, every click, hover and screenshot costs a flat **five seconds** instead of
   under a fifth of one, and the same occlusion is behind the frozen DOM, the dead input and
   the timed-out screenshots in pitfalls. One request at the start saves minutes and most of
   the run's failure modes; see the measurements in app-map.

If a precondition needs the user (e.g. switching a branch out from under a parallel session,
or raising the Chrome window), stop and ask.

### Start each run on a freshly paired device

Do not inherit the pairing from an earlier run. It is lost whenever the app's caches are
rebuilt, and a device carried between runs can bring a jammed upload queue with it (see
pitfalls). Pairing codes are single-use, so make a new one, then pair:

```bash
# Creates a device node holding the code, first clearing that code off any node that
# still has it (codes must be unique). Verified 2026-08-21.
ddev drush eval "\$code='88888888'; \$title='QA Manual Device '.date('Ymd-His'); \
variable_set('hedley_super_user_mode',1); \$q=new EntityFieldQuery(); \
\$r=\$q->entityCondition('entity_type','node')->propertyCondition('type','device') \
->fieldCondition('field_pairing_code','value',\$code)->execute(); \
if(!empty(\$r['node'])){foreach(node_load_multiple(array_keys(\$r['node'])) as \$o){ \
\$o->field_pairing_code[LANGUAGE_NONE][0]['value']=''; node_save(\$o);}} \
variable_set('hedley_super_user_mode',0); \
\$n=entity_create('node',array('type'=>'device','title'=>\$title)); \
\$w=entity_metadata_wrapper('node',\$n); \$w->field_pairing_code->set(\$code); node_save(\$n); \
echo 'created: '.\$title.' code='.\$code;"
```

Then in the app: if it already shows the PIN page, wipe the local state first so the pairing
screen comes back (unregister the service worker, delete the caches **including `config`**,
delete IndexedDB, clear storage, reload — see pitfalls). Enter the code with `form_input` on
the field's ref, never by clicking and typing. After pairing, general data has to download
before any PIN is accepted: "Your PIN code was not recognized" straight after pairing means
sync has not finished, not that the PIN is wrong.

### What the browser tools cannot set up — ask the user

Two things worth having that this skill cannot do for itself. Say so rather than pretending:

- **An incognito window.** No browser tool opens one, and a Chrome extension is inert in
  incognito unless the user has ticked "Allow in Incognito" for it. The clean-slate benefit
  is available anyway through the local-state wipe above, so treat incognito as optional:
  mention it, and only pursue it if the user sets the window up and hands it over.
- **iPad Mini (or any device) emulation.** Tabs driven through these tools already run under
  a fixed emulated viewport — 1200×1799 CSS px at devicePixelRatio 1, with `outerWidth`
  reading 0 and `ontouchstart` false. `resize_window` changes the OS window but leaves
  `innerWidth`/`innerHeight` untouched, so the tablet metrics, the pixel ratio and touch
  events are all out of reach (measured 2026-08-21). Anything that depends on a real tablet
  viewport — layout at 768 px, touch-only gestures — has to be checked by hand on a tablet,
  and the run should say so instead of implying it was covered.

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
- **Inject the cursor before recording anything.** `cursor.js` next to this skill draws the
  pointer and the last three clicks into the page, the way the e2e videos do it
  (`client/e2e/helpers/cursor.ts`). Without it a viewer cannot tell where a click landed:
  gif_creator's own overlay marks only the frame captured at the instant of the click, and
  QA frames are taken a second or more later. Pass the file's contents to `javascript_tool`
  and **re-inject after every navigation** — a page load wipes it. It survives Elm's
  re-renders on its own.
- **Hover, pause, then click** — the same shape as the e2e `click()` helper. The hover frame
  shows the pointer resting on the control before it is pressed, so the recording reads as
  someone using the app rather than as things happening by themselves.
- **Record every scenario, and keep it as `.mp4`.** `gif_creator` is the only capture there
  is, so record with it — start capturing before the scenario's first action, take extra
  screenshots around each action (they are the frames, and they double as the flush the
  occluded-window pitfall needs), and export as `qa-<pr>-<scenario>.gif` with gif_creator's
  own click overlay turned **off** — the injected cursor already marks every click, and two
  markers in one frame is just noise:

  ```
  options: { showClickIndicators: false, showActionLabels: false, showDragPaths: false }
  ```

  Its progress bar and watermark are worth keeping. Chrome saves
  exports to `~/Downloads`. Convert each one to .mp4 in `client/qa-recordings/<pr>/`
  (gitignored, like the e2e recordings) and delete the GIF, right after exporting and before
  the next scenario:

  ```bash
  # Absolute output path — the command must not depend on the working directory.
  mkdir -p /var/www/html/ihangane/client/qa-recordings/<pr>
  ffmpeg -y -loglevel error -i ~/Downloads/qa-<pr>-<scenario>.gif \
    -movflags +faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" \
    -c:v libx264 -crf 23 \
    /var/www/html/ihangane/client/qa-recordings/<pr>/qa-<pr>-<scenario>.mp4 \
    && rm ~/Downloads/qa-<pr>-<scenario>.gif
  ```

  Then check the file rather than assuming: `ffmpeg -v error -i <file> -f null -` must print
  nothing, and a frame pulled from the moment that matters
  (`ffmpeg -ss <t> -i <file> -frames:v 1 frame.png`) must show the thing being demonstrated,
  legibly. A recording that converted cleanly but does not contain the warning is not
  evidence.
- **The recording is not real time — say so when handing it over.** gif_creator captures one
  frame per action and gives every frame the same duration on export, so playback speed is a
  function of the frame count, not of the clock. The pairing run was 23 frames held 0.89s
  each = 20.4s of video for about four minutes of real work, and a six-second wait for sync
  looks exactly like a one-second pause. Never let a reader infer timing, responsiveness or
  "how long the app took" from one of these files. If a scenario is *about* timing, capture
  it differently: screenshot with `save_to_disk: true`, note the wall-clock time of each
  frame, and assemble with ffmpeg's concat demuxer giving each frame its real duration.

  Same format the e2e recordings are kept in, and about an eighth of the size (49 MB of GIF
  became 6.2 MB of mp4 on the #2007 run). The overlays gif_creator burns in — click rings,
  action labels, progress bar — survive the conversion. The `scale` filter is not optional:
  the capture height is odd and `yuv420p` needs even dimensions. These recordings ARE the
  demonstration of the verification; a scenario without one counts as not verified.
- The capture stops at **50 frames**. Watch the count: on a long scenario, export and start
  a fresh recording once the important moment is captured, rather than losing the end.
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
clickable link (`file:///var/www/html/ihangane/client/qa-recordings/<pr>/qa-<pr>-<scenario>.mp4`)
so they can watch the verification. For failures, include the drafted PR comment or issue
text from "When you find a bug" and ask for approval to post it.

Then run the learning loop from the top of this file (ledger, pitfalls, app-map).
