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
