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
- **Rule:** after a server DB reinstall, a browser device's queued uploads reference UUIDs
  that no longer exist, and the FIFO upload lane jams permanently. Recover by wiping the
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
