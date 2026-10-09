# QA Pitfalls — E-Heza

Mistakes made (or nearly made) during QA runs: symptom → wrong conclusion → rule. Read all
of them before every run; add new ones only at the end of a run. Pitfalls that belong only
to driving through the Chrome extension are in `chrome-extension.md`.

## Testing a stale build

- **Symptom:** the change is invisible in the app although the diff clearly adds it.
- **Wrong conclusion:** the feature is broken.
- **Rule:** check, in order: (1) the main tree is on the PR's branch — gulp serves only that
  tree; (2) gulp has finished compiling; (3) the build in the browser doing the run leaves
  nothing out: `git diff --quiet <version> -- client/src`. Each browser profile caches its
  own build. Comparing the version with HEAD raises false alarms — bookkeeping commits move
  HEAD without touching the client.

## A PR's commit list can contain work a later commit undoes

- **Symptom:** the plan is built from commit messages, and the behaviour is not in the app.
- **Wrong conclusion:** the feature is broken, or the build is stale.
- **Rule:** commit subjects describe intent at the time, not the merged result. Check the
  merged tree for the identifier a commit introduced (`git grep <symbol> <commit> -- client/src`).
  A late "Leave X where it is" or "Take out …" is usually a revert. Plan against the code.

## Screen unreachable

- **Symptom:** a menu item or activity the diff touches does not appear anywhere.
- **Wrong conclusion:** navigation is broken.
- **Rule:** check whether it is behind a feature flag or limited to a role (nurse, CHW, lab
  technician — see app-map) before calling it unreachable.

## Concluding from the backend too early

- **Symptom:** a node the UI should have created is missing in Drupal, or a saved edit sits
  in `shardChanges` forever.
- **Wrong conclusion:** sync, the encoder or the backend handler is broken.
- **Rule:** backend effects land only after an upload round, and two gates stand before it:
  the health centre's START SYNCING, and the upload lane's idle time (app-map). Run
  `h.common.syncAndWait(page)`, then query.

## Reading the wrong IndexedDB store

- **Symptom:** after a successful health-centre sync, `nodes` holds no persons.
- **Wrong conclusion:** the authority sync downloaded nothing.
- **Rule:** `nodes` holds general data only. Persons, relationships and measurements are in
  `shards`; count there.

## Stale device with a poisoned upload queue

- **Symptom:** the Error log repeats `Could not find UUID: <uuid>`; nothing uploads or
  downloads; the last successful contact is old.
- **Wrong conclusion:** the backend or the PR broke sync.
- **Rule:** after a server reinstall, a device's queued uploads point at UUIDs that no longer
  exist and the upload lane jams for good. Treat "the server was reinstalled" as an order to
  start the driver with `--fresh`, not as something to discover through sync errors.

## The loading screen is the pairing screen, and it can last 20 seconds

- **Symptom:** after a reload the app shows "This device has not yet been authorized…" with
  a pairing-code field.
- **Wrong conclusion:** the pairing was lost; mint a new code and pair again.
- **Rule:** that is what the app renders while it restores itself from IndexedDB, for up to
  15–20 s. Wait and read the state again. A code spent on this is wasted.

## Calling a slow load a known bug, and writing off a plan row

- **Symptom:** a section shows "The server indicated the following error: Not Found" through
  several reloads, and it matches a defect already in memory.
- **Wrong conclusion:** it is that defect; report the row BLOCKED. It loaded on a later
  reload and the row passed.
- **Rule:** a matching memory raises the prior; it does not close the question. Before
  calling a row blocked, reload several times over a minute or more. Data present in
  `shards` proves the app can show it. Report "not reproducing yet", not a bug's name.

## Planning a Case Management scenario without reading the pane's clearing rule

- **Symptom:** the follow-up entry the scenario needs vanishes the moment the encounter is
  created.
- **Wrong conclusion:** the follow-up was lost, sync ate it, or the PR removed the entry.
- **Rule:** each pane decides for itself whether an encounter clears its entry. Read the
  pane's rule and the table in app-map before choosing a pane.

## Picking one of several repeated blocks by walking up from a button

- **Symptom:** "the START SYNCING button of Nyange" picked by an ancestor containing "Nyange"
  clicked Muhondo's button.
- **Wrong conclusion:** the selector did not match. It matched too well: a common ancestor
  holds every block's text.
- **Rule:** scope to the container holding exactly one block —
  `page.locator('.health-center', { has: page.locator('h2', { hasText: 'Nyange' }) })` — and
  confirm afterwards which one changed.

## Judging a new UI element by its text instead of looking at it

- **Symptom:** a new dialog was signed off because its text and behaviour were right; the
  user then saw its button sticking 16 px out of the modal.
- **Wrong conclusion:** "the dialog was verified". Its content and behaviour were; its
  appearance never was.
- **Rule:** when a PR adds new UI, open the nearest existing sibling and compare geometry,
  not just text — `getBoundingClientRect()` on the element and its container. In E-Heza
  modals, a `fluid` button placed directly in `div.actions` always overhangs (Semantic's
  `.ui.modal .actions > .button` margin); correct dialogs wrap it in `div.two.ui.buttons`.

## Asserting on a marker the app never renders

- **Symptom:** a "nothing is selected" check passes on the fixed build and on the broken one.
- **Wrong conclusion:** the behaviour is verified.
- **Rule:** before trusting an absence, prove the marker ever appears. Yes/no inputs mark the
  chosen side on the `input` (`checked`), never on the `label`, so `label.active` is always
  absent. Read the Elm view to decide what to assert on.

## Building a fixture by hand when a helper can build it

- **Symptom:** the run spends its budget on registration and a dozen activities before it
  reaches the one screen under test.
- **Wrong conclusion:** manual QA is not worth it for this change.
- **Rule:** setup is a chain of e2e helpers in one driver command; the fixture is still made
  through the real UI. Spend the care on the screen under test. Never leave files in
  `client/e2e/` — anything there joins a CI job.

## Restarting the driver loses what was typed

- **Symptom:** after `qa.sh stop` / `start`, a form that had answers shows them empty.
- **Wrong conclusion:** the app dropped the input.
- **Rule:** a restart reloads the page; only what was saved survives. Read the state before
  carrying on, and restart only between steps that end in a Save.

## Guessing node type names in a backend query

- **Symptom:** after a successful sync, `h.common.queryMeasurementNodes(name, ['height', …])`
  reports every measurement missing.
- **Wrong conclusion:** the upload failed.
- **Rule:** measurement node types carry the module's prefix (`nutrition_height`, not
  `height`), so guessed names match nothing. Use the module's own query helper (`h.nutrition.queryBackendNodes(name)`) or
  read the types from the e2e spec for that encounter.
