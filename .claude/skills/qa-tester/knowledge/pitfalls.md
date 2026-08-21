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
