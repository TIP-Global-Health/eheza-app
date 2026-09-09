# QA Pitfalls — E-Heza

Mistakes made (or nearly made) during QA runs. Each entry: symptom → wrong conclusion → rule.
Read all entries before every run; append new ones only at the end of a run.

## Native dialogs freeze the browser extension

- **Symptom:** after clicking a Delete-style button, every browser tool call hangs.
- **Wrong conclusion:** the extension or the app crashed.
- **Rule:** native `confirm()`/`alert()` dialogs block the Chrome extension entirely. Never
  click elements that trigger them; find another route or ask the user to click through
  manually (they must also dismiss the dialog if one already opened).

## Testing a stale build

- **Symptom:** the change is invisible in the app although the diff clearly adds it.
- **Wrong conclusion:** the feature is broken.
- **Rule:** three staleness sources, check in order: (1) the main tree is on a different
  branch than the PR — gulp serves only the main tree; (2) gulp has not finished compiling;
  (3) "Version" was not clicked after the recompile, so the service worker still runs the
  previous build.

## Concluding from the backend too early

- **Symptom:** an expected node/report row is missing in Drupal right after acting in the UI.
- **Wrong conclusion:** sync or the backend handler is broken.
- **Rule:** the app is offline-first; backend effects land only after the sync upload lane
  drains. Wait for the sync indicator to go idle before querying the backend.

## Screen unreachable

- **Symptom:** a menu item or activity the diff touches does not appear anywhere.
- **Wrong conclusion:** navigation is broken.
- **Rule:** check first whether the screen is behind a feature flag (see app-map) or
  restricted to a role (nurse vs CHW vs lab tech — some encounter types are role-exclusive;
  see the e2e knowledge base) before reporting it unreachable.

## Occluded Chrome window: the one root cause behind four symptoms

Before treating any of the four as its own problem, check `document.hidden`. A hidden window
gives: a frozen DOM (below), screenshots that time out after 30 s, clicks that never reach
the page, and a flat 5 s cost on every click, hover and screenshot instead of ~0.2 s
(measured both ways — see app-map). Raising the window is the fix for all of them, and it is
the user who has to do it: `resize_window` changes the OS window without clearing `hidden`.

## Occluded Chrome window freezes the app under test

- **Symptom:** clicks change the URL hash but the page never re-renders; `form_input` on a
  select "succeeds" but the dependent dropdown stays empty; sync makes no network requests.
- **Wrong conclusion:** the Elm app crashed, or the form wiring is broken.
- **Rule:** when the Chrome window is covered by the terminal, `document.hidden` is true:
  Chrome suspends requestAnimationFrame (Elm renders through it — the DOM freezes while the
  model advances) and intensively throttles timers (the SyncManager stops ticking). Two
  standing workarounds: (1) take a screenshot after every interaction — the CDP capture
  forces a compositor frame and flushes Elm's pending render; (2) run
  `navigator.locks.request('qa-keepalive', () => new Promise(() => {}))` once per page load —
  a held Web Lock exempts the page from intensive timer throttling so sync keeps running.
  Verify state via JS/DOM queries after the screenshot, not before.

## Never click a native select with the mouse

- **Symptom:** after clicking a `<select>`, every browser tool fails with "Cannot access a
  chrome-extension:// URL of different extension"; even screenshots and JS are blocked.
- **Wrong conclusion:** the extension crashed.
- **Rule:** the OS-native dropdown popup traps the automation focus like a native dialog and
  nothing can dismiss it programmatically (new tabs don't help; Wayland has no key injector).
  The only recovery is closing the tab (login and IndexedDB state survive). Always set
  selects with `form_input` on a freshly-found ref instead of mouse clicks.

## form_input skips the event when the value already matches

- **Symptom:** `form_input` reports `Selected option "X" (previous: "X")` and the app never
  reacts, even though the app state clearly does not reflect X.
- **Wrong conclusion:** the app ignores the field.
- **Rule:** if a previous (lost) attempt already set the DOM value, `form_input` sees no
  change and fires no event. Toggle through a different value first, or reset the value to
  `''` via JS, then set the intended one.

## Stale device with a poisoned upload queue

- **Symptom:** sync errors repeating `Could not find UUID: <uuid>` in the app's Error log;
  last successful contact weeks old; nothing uploads or downloads.
- **Wrong conclusion:** the backend or the PR under test broke sync.
- **Rule:** after a server DB reinstall — which also happens whenever someone runs
  `server/install` — a browser device's queued uploads reference UUIDs that no longer exist,
  and the FIFO upload lane jams permanently. Treat "the server was reinstalled" as an order
  to wipe the browser before the next run, not as something to discover through sync errors. Recover by wiping the
  app's local state completely and re-pairing a fresh device node: unregister the service
  worker, delete the caches **including the `config` cache** (it holds the pairing/robot
  credentials and the sync baseline — deleting only the `sync` IndexedDB leaves the app
  thinking it is already synced), delete IndexedDB, clear storage, reload. Create the device
  node with a known pairing code via drush (see e2e `helpers/device.ts` for the PHP).

## gh issue/pr view fails on this repo

- **Symptom:** `gh issue view` / `gh pr view` exit with a Projects-classic GraphQL error.
- **Wrong conclusion:** the issue does not exist or gh is misconfigured.
- **Rule:** use the REST API instead: `gh api repos/TIP-Global-Health/eheza-app/issues/<n>`
  (and `.../pulls/<n>`, `.../issues/<n>/comments`).

## A PR's commit list can contain work a later commit in the same PR undoes

- **Symptom:** the test plan is built from the PR's commit messages, and the behaviour
  the plan is about is simply not there in the app.
- **Wrong conclusion:** the feature is broken, or the build is stale.
- **Rule:** on a long PR, commit subjects describe intent at the time, not the merged
  result. Before planning, check the **merged tree** for the identifier the commit
  introduced (`git grep <newSymbol> <mergeCommit> -- client/src`). A subject like
  "Leave X where it is" or "Take out ..." late in the list is usually a revert of an
  earlier one. Plan against the code that merged, not against the story of getting there.

## Clicking a text field the browser can autofill locks the extension out of the tab

- **Symptom:** after clicking a field and typing, every browser tool fails with
  "Cannot access a chrome-extension:// URL of different extension" — screenshots too.
- **Wrong conclusion:** the extension crashed, or the app hung.
- **Rule:** an autofill popup traps automation focus the same way a native select does.
  It happens on the PIN field and on ordinary text fields the browser has saved values
  for — the registration form's First Name did it after a few registrations. Set such
  fields with `find` + `form_input` on the field's ref rather than clicking and typing.
  Recovery is the same as for selects: close the tab (pairing and IndexedDB survive),
  reopen the URL you were on — the app restores the page from the hash.

## Screenshot timing out is the occlusion pitfall, not a frozen app

- **Symptom:** `Page.captureScreenshot` times out after 30s, while `javascript_tool`
  still answers and reports `document.hidden: true`; clicks appear to do nothing.
- **Wrong conclusion:** the renderer is frozen.
- **Rule:** the tab is fully hidden (window covered or not the active tab), so there is
  no compositor to capture from and Elm's requestAnimationFrame render never runs.
  `resize_window` brings the window back and screenshots start working again; the Web
  Lock keepalive does not help with this one.

## Reading a dependent dropdown before the DOM has caught up

- **Symptom:** a cascading select (District after Province, MONTH after YEAR) reads as
  empty in JS right after the parent was set, so the run concludes the cascade is broken.
- **Wrong conclusion:** Elm ignored the change event.
- **Rule:** this is the occluded-window freeze again — Elm's model advanced, the DOM had
  not. Setting selects from JS works (`value` then `input` **and** `change` events, both
  bubbling); what is missing is the render. Take a screenshot to force the frame, then
  read the dependent options. Also note that changing YEAR rebuilds the MONTH list, so
  set the year first and re-read the month options before choosing one.

## Clicks stop reaching the page while the tool still reports success

- **Symptom:** `computer left_click` returns "Clicked at (x, y)" — by coordinate or by ref —
  and nothing happens. The app does not advance, and the injected cursor does not move
  either, which is the giveaway: even a plain mousemove is not arriving.
- **Wrong conclusion:** the coordinates are wrong, the click is landing on an overlay, or
  the app is busy. Re-clicking, re-finding the ref and resizing the window all fail the
  same way, which burns a lot of the run.
- **Rule:** prove it in one call before theorising. Install counters —
  `window.__ev={move:0,down:0}; document.addEventListener('mousemove',()=>__ev.move++,true);
  document.addEventListener('mousedown',()=>__ev.down++,true)` — then click and read
  `window.__ev`. Both still 0 means input delivery to that tab is dead, not a targeting
  problem. `document.elementFromPoint` on the target's real rect will happily confirm the
  button is on top and clickable, which is a red herring. Recovery is the usual one: close
  the tab and reopen the URL (pairing and IndexedDB survive). Export whatever the recording
  already holds first — the frames captured up to that point are still good evidence.
  Seen twice, both times at the **health-centre choice right after a nurse signs in on a
  freshly paired device**, while the first sync is still running: the OS pointer still moves
  on screen but the page's own listeners never fire. Treat that screen on a fresh pairing as
  a known place to lose input, and plan the recording so the scenario's evidence is captured
  before it.

## The extension can drive a phantom tab the user never sees — CONFIRMED CAUSE

- **Confirmed 2026-08-27**, after a long detour through window-occlusion and Wayland theories
  that were all wrong. The claude-in-chrome tab group contained a tab that Chrome never
  displays. The user saw a "Claude" tab group whose Google tab looked normal; the tab the
  extension was driving was a *different* one, on the same URL.
- **The one-call diagnostic** — do this first, before any theorising about windows:

  ```js
  document.title = '### CLAUDE IS DRIVING THIS TAB ###';
  const b = document.createElement('div');
  b.style.cssText = 'position:fixed;inset:0;z-index:2147483647;background:#e4002b';
  document.documentElement.appendChild(b);
  ```

  Ask the user what they see. Normal page = phantom tab, and **no amount of window arranging
  will ever fix it**. A user who cannot find a tab you just renamed is telling you this
  outright; treat it as proof, not as a puzzle about window ordering.
- **The recorder that proves it without timing games:** install a `visibilitychange` listener
  plus a free-running `requestAnimationFrame` counter, ask the user to look at the tab for a
  few seconds, then read both back. An empty transition log and a frame count that never moved
  means the tab was never shown at all — no argument about when the measurement landed.
- **The fix is a reinstall of the extension — confirmed 2026-08-27.** Nothing else worked:
  raising the window, `resize_window`, a fresh tab, a new tab group, side-by-side arrangement,
  a full Chrome restart, and a whole machine reboot all left it at 0 fps. Removing and
  reinstalling Claude in Chrome fixed it immediately and completely — new `deviceId`, and the
  first new tab measured **40 frames / 61 fps, `visible`, hover 22 ms** against 0 fps and
  5011 ms before. So: propose the reinstall early rather than treating it as a last resort,
  and use chrome-devtools-mcp only to keep working while the user gets to it.

## The unrenderable tab: symptoms and what does NOT fix it

- **Symptom:** `document.hidden` true with `visibility: "hidden"` while `hasFocus()` is true, a
  flat 5 s on every click/hover, and — the decisive measurement — **`requestAnimationFrame`
  fires 0 frames in 2 s**.
- **Wrong conclusion:** the previous entries' advice will get you out of it. It did not:
  asking the user to raise the window, `resize_window`, creating a fresh tab, a whole new tab
  group, and a full Chrome restart all left it at 0 fps. The user could not find the tab at
  all — `Ctrl+Shift+A` tab search returned nothing for a title I had set on it, even though
  `chrome.tabs` reported that title back to me.
- **Also disproven on this machine:** arranging Chrome side-by-side with the terminal so it is
  plainly visible. Still 0 fps with `focus: true`. Six attempts total — raising the window,
  `resize_window`, a fresh tab, a new tab group, a full Chrome restart, side-by-side — none
  moved it, so treat it as an environment-level defect on this station rather than something
  to keep poking at. Worth trying once, as a config change the user makes: launch Chrome with
  `--ozone-platform=x11` instead of the current `--ozone-platform=wayland`.
- **It is not the app.** Navigating the same tab to `https://www.google.com` gives the identical
  reading — 0 frames, `visibility: "hidden"`, 5011 ms hover. Site-independent, so never spend
  time suspecting the E-Heza build, its service worker or the localhost origin: one navigation
  to any public page settles it in a single call.
- **A screenshot is not proof of rendering.** `computer screenshot` returned a correct, current
  image in 637 ms while rAF had been dead for hours. Capture and paint are different paths;
  only the rAF count tells you Elm will re-render.
- **Rule:** measure rAF, not `document.hidden`, to decide whether the page is really rendering
  — `frames > 0` is the gate before driving anything. If it is 0 and one round of raising the
  window does not fix it, **stop trying to fix the window** and switch drivers: the
  `chrome-devtools-mcp` server drives its own headed Chrome that renders at 62 fps
  (`visibility: "visible"`), on a fresh profile, and can do everything this skill needs except
  `gif_creator`. Assemble recordings from `take_screenshot filePath=…` frames with ffmpeg's
  concat demuxer instead. Correcting the earlier entry: `resize_window` does **not** reliably
  bring the window back.

## Reading the wrong IndexedDB store and concluding sync failed

- **Symptom:** after a health-centre sync reports "Status: Success, Remaining for Download: 0",
  a scan of the `nodes` object store finds 0 persons, so sync looks broken.
- **Wrong conclusion:** the authority sync did not download anything.
- **Rule:** `nodes` holds only **general** entities (nurses, villages, health centers,
  counseling topics — 36 of them on a fresh pair). Authority data (persons, relationships,
  measurements) lands in the **`shards`** store. Count there before concluding anything about
  an authority sync.

## Clicking a menu card's label instead of its handler

- **Symptom:** clicking "DEVICE STATUS" / "CLINICAL" on the main menu — by a11y uid, by
  coordinate, or with `.click()` on `div.card` — does nothing at all, repeatedly.
- **Wrong conclusion:** input delivery to the tab is dead (the existing pitfall), or the app
  is frozen.
- **Rule:** `activityCardWithCounter` (`Utils/Html.elm`) puts `onClick` on the inner
  **`div.image`**, the grey icon square — not on `div.card` and not on the label under it.
  Click `card.querySelector('div.image')`. Check where the handler actually sits in the Elm
  source before blaming the environment; the label is a sibling of the clickable element.

## Waiting for a sync that was never going to run

- **Symptom:** a saved edit sits in `shardChanges` forever; polling for the upload finds
  nothing, so the encoder looks like it is not producing a payload.
- **Wrong conclusion:** the change was not queued, or the upload path is broken.
- **Rule:** two separate gates. (1) After pairing, each health centre on Device Status starts
  at **"START SYNCING"** — the authority downloads nothing until that is clicked. (2) The
  upload lane otherwise waits out its idle time (default 600 s), so click **"TRY SYNCING WITH
  BACKEND"** to make an upload happen now. Neither is a defect; budget both into any scenario
  that ends in "and then it syncs".

## Calling a slow load a pre-existing bug, and writing off a plan row

- **Symptom:** a section renders "The server indicated the following error: Not Found" on every
  person that has data, survives two reloads, a service-worker update and three different
  persons — and it matches a defect already written up in memory
  (`clinics-not-found-bug.md`), which even records it as a deliberate non-fix.
- **Wrong conclusion:** it *is* that defect, the plan row that depends on it is unreachable,
  report the row BLOCKED and move on. I did exactly this, wrote it into the ledger and the
  report, and the user then asked me to refresh again — it loaded, and the row passed.
- **Rule:** a matching entry in memory raises the prior, it does not close the question. Before
  declaring any row blocked, exhaust the cheap retry first: several **full** reloads spaced
  over a minute or more, not the three-in-twenty-seconds I did. Data that arrived in the
  `shards` store is proof the app *can* show it, so a UI that does not yet is a timing
  statement, not a verdict. And say "not reproducing yet" in the report rather than naming a
  known bug — naming one makes a guess look settled and invites everyone downstream to skip
  the retry too.

## Planning a Case Management scenario without reading the pane's own clearing rule

- **Symptom:** the follow-up entry used to set a scenario up vanishes from the pane the moment
  the encounter is created, so the state the plan needed ("an encounter of that type exists
  today, and the entry is still tappable") cannot be reached at all.
- **Wrong conclusion:** the follow-up measurement was lost, sync ate it, or the change under
  test removed the entry. None of that: each pane decides for itself whether an encounter
  clears its entry, and the panes disagree.
- **Rule:** before choosing a pane to demonstrate a Case Management state, read that pane's
  `generate<Type>FollowUpEntryData` in `Pages/GlobalCaseManagement/View.elm` and note which
  `limitDate` its `view<Type>Pane` passes in. The two shapes seen so far:
  - **Nutrition** (and Acute Illness, Prenatal) pass `limitDate = currentDate + 1`, so
    **today's** encounter is included in the comparison and clears the entry
    (`encounter.startDate < item.dateMeasured` fails when both are today). A patient can
    never be tapped there on a day that already has an encounter.
  - **Immunization** passes `limitDate = currentDate`, so today's encounters are filtered out
    of the comparison entirely and the entry survives the same-day encounter. This is the only
    pane where "an encounter already took place today" is reachable.
  Acute Illness additionally needs the follow-up to belong to the LAST encounter of the
  illness, and needs a resolvable diagnosis — an encounter that ends with no diagnosis
  produces no entry at all.

## A long-running javascript_tool body loses the extension connection

- **Symptom:** a `javascript_tool` call containing a multi-round loop (each round clicking,
  saving and sleeping ~2 s) returns "Browser extension is not connected. Please ensure the
  Claude browser extension is installed and running".
- **Wrong conclusion:** the extension broke or the phantom-tab defect is back. It had not —
  `tabs_context_mcp` reconnected immediately and the page had actually advanced two sub-tasks
  before the call died.
- **Rule:** keep a `javascript_tool` body under roughly ten seconds of wall clock. Drive long
  form sequences as several short calls (batched in one `browser_batch` when they do not need
  each other's results), and after any such error call `tabs_context_mcp` and re-read the page
  state before redoing anything — part of the work is usually already done.

## Selecting one of several repeated blocks by walking up from a button

- **Symptom:** targeting "the START SYNCING button belonging to Nyange Health Center" by
  walking each button's ancestors until one contains "Nyange" clicked **Muhondo's** button.
- **Wrong conclusion:** the selector did not match. It matched too well: a few levels up, the
  common ancestor contains every health centre's text, so the first button tested passes.
- **Rule:** an ancestor-text test only identifies a block when the text is unique to it. Pick
  repeated controls by their index among siblings, or by the nearest container that holds
  exactly one of them — and after clicking, confirm which one actually changed state.

## Screenshot coordinates after the window geometry changes on its own

- **Symptom:** a click computed from a fresh screenshot lands at the edge of its target and
  does nothing; the next screenshot comes back a different size (1568x733 rather than 920x1336).
- **Wrong conclusion:** the click was mis-aimed, or input delivery died.
- **Rule:** the emulated viewport is NOT fixed across a session — it changed here without any
  `resize_window` call, and it differed from the 1200x1799 recorded on earlier runs. Click by
  `ref` wherever `find` exposes the element. For E-Heza's clickable `div`s, which the a11y tree
  does not expose, read the element's own rect and scale it in the same call that uses it:
  `r.left + r.width/2` times `1568/innerWidth`. Never carry coordinates across a navigation.

## Verifying a new UI element by its text instead of looking at it

- **Symptom:** a new dialog is signed off as correct because `modal.innerText` reads exactly the
  expected sentence and the expected button label, and the behaviour behind it is right. The
  user then looks at the same recording and immediately sees the button is misaligned —
  16 px right of the modal, overhanging its edge.
- **Wrong conclusion:** "the dialog was verified". Its *content* and *behaviour* were; its
  *appearance* never was. Having the frame on screen is not the same as examining it: the
  frame was used only to confirm the sentence was legible.
- **Rule:** when a PR adds a NEW piece of UI, verify it against the nearest existing sibling
  component, not against its own spec — open both, and compare geometry, not just text. The
  cheap mechanical check is to measure: `getBoundingClientRect()` on the new element and on its
  container, and assert the edges line up the way the sibling's do. For E-Heza modals
  specifically, Semantic ships `.ui.modal .actions > .button { margin-left: .75em }`, a
  **direct-child** rule — so a `fluid` button placed straight into `div.actions` is always
  100% wide *plus* 0.75em of left margin and always overhangs. Every correct dialog in
  `Pages/GlobalCaseManagement/View.elm` avoids it by wrapping the button in
  `div [ class "two ui buttons" ]`, which is also what makes a lone button render full width
  (`ui buttons` and `one ui buttons` both collapse it to its text width instead — measured).

## The loading screen is the pairing screen, and it can last 20 seconds

- **Symptom:** right after a reload, the app shows "This device has not yet been authorized to
  sync data with the backend…" with a pairing-code field, so the pairing looks lost.
- **Wrong conclusion:** the reload or the service-worker update dropped the pairing — mint a
  new code and pair again.
- **Rule:** that Device Status text is what the app renders *while it hydrates* from IndexedDB.
  On this build it persisted 15–20 s after a reload and then resolved to the restored session,
  same device and same signed-in nurse. Wait and re-read (or take a screenshot to force the
  frame) before concluding anything; a pairing code spent on this is wasted, and codes are
  single-use.
