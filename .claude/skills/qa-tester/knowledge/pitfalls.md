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

## Cutting the top off a reply hides a failed command

- **Symptom:** a command's reply, piped through `sed -n '/"state"/,$p'` or `head` to keep it
  short, showed a plausible screen; the next step then found the page was not where it was
  meant to be.
- **Wrong conclusion:** the click ran and the app did something else.
- **Rule:** `"ok"` and `"error"` are the first lines of every reply. Never filter them out;
  trim only the end. A range such as `/"log"/,$p` prints nothing when the run logged nothing,
  and the checks go with it: save the reply to a file and print `ok`, `error`, `value` and `checks`.

## Planning several rows on one fixture without checking how they interact

- **Symptom:** the plan reused one acute illness encounter for a malaria row and a COVID
  row. After the malaria result was changed to Positive, the COVID test was gone.
- **Wrong conclusion:** the PR removed the COVID test.
- **Rule:** a row changes the record the next row starts from. While planning, read the
  conditions that show each row's screen (`expect…Task` in the module's `Utils.elm`)
  against the state the earlier rows leave, and order the rows, or split fixtures, to suit.

## Staged files ride along with the hook's bookkeeping commit

- **Symptom:** `git checkout <ref> -- <files>` put a PR's files in the main tree — and in
  the index.
- **Wrong conclusion:** working-tree changes stay local until someone commits them.
- **Rule:** the `Stop` hook runs `git commit`, which takes everything staged. After any
  checkout of paths, `git reset -q -- <files>` before the turn ends.

## Backdating a visit leaves its measurements dated today

- **Symptom:** after `h.common.backdateEncounter` and a sync, a form at the next visit says
  "No previous measurement on record", although the earlier visit holds a value.
- **Wrong conclusion:** the PR broke the previous-value lookup.
- **Rule:** the helper moves only the encounter's `field_scheduled_date`. Each measurement
  keeps its own `field_date_measured`, and "previous" lookups read that — so the value
  still counts as today's. Move the encounter's measurements to its date too (drush:
  set `field_date_measured`, `node_save`), sync, and confirm the date on the device
  before trusting any "previous" or "no previous" result.

## Chaining registration helpers from the wrong screen

- **Symptom:** a second `create…AndStart…Encounter` in one command fails waiting for
  `.icon-task-clinical`.
- **Wrong conclusion:** the app or the helper is broken.
- **Rule:** registration helpers start from the main menu. Between fixtures,
  `page.goto('/')` and wait for `.wrap-cards` first.

## Testing a "page loads its data" fix with the data already loaded

- **Symptom:** a fix for a page that never fetched something passes — but the data had
  just been entered on the same tablet, so it was in the app's memory anyway.
- **Wrong conclusion:** the fix is verified.
- **Rule:** when a PR adds a fetch, reload the app (`page.goto('/')`) between creating the
  data and opening the page under test, so the page has to load it itself. The recording
  carries on through a reload.

## A `sed` or `head` without a file waits for input

- **Symptom:** a Bash call that ran the driver fine never returns and is moved to the
  background.
- **Wrong conclusion:** the driver hung.
- **Rule:** every filter in a chain needs its input — a file or a pipe. A stray
  `sed -n '…'` with neither reads the terminal forever.

## Guessing node type names in a backend query

- **Symptom:** after a successful sync, `h.common.queryMeasurementNodes(name, ['height', …])`
  reports every measurement missing.
- **Wrong conclusion:** the upload failed.
- **Rule:** measurement node types carry the module's prefix (`nutrition_height`, not
  `height`), so guessed names match nothing. Use the module's own query helper (`h.nutrition.queryBackendNodes(name)`) or
  read the types from the e2e spec for that encounter.

## Overlaying from a merge base with a stale local develop

- **Symptom:** the overlay copied `package.json`, `package-lock.json`, `gulpfile.js` and `elm.json` along with the PR's five Elm files, and overwrote a lock file the user had modified locally.
- **Wrong conclusion:** `git diff $(git merge-base develop $b) $b` lists the PR's files.
- **Rule:** local develop can be behind origin, so the merge base drags in commits already on origin/develop. Take the file list from `gh pr diff <n> --name-only`, and check `git status --short -- $files` is empty before checking out over them.

## A scripted step after a helper that already finished the flow

- **Symptom:** prenatal Next Steps ended on the main menu, and the script's own "Pause Encounter" click then waited 10 s and failed, leaving a dead tail on the video.
- **Wrong conclusion:** the pause did not happen.
- **Rule:** read what a helper does at its end before adding a step after it. `completeNextSteps` presses Pause Encounter on the Wait task itself.

## Trusting the form straight after Save

- **Symptom:** after Save, the reopened form showed the entered values, and the run called them saved.
- **Wrong conclusion:** the data is stored, uploaded and readable by other devices.
- **Rule:** the form after Save shows the clicks. Prove the upload (Remaining for Upload: 0), reload and reopen it, then re-pair and open it again (SKILL.md, Proving the data was saved).

## Checking for an activity card before the encounter page has drawn it

- **Symptom:** straight after closing the progress report, a script found no History card in To Do, switched to Completed, and then failed there.
- **Wrong conclusion:** the activity moved to Completed, or the app lost it.
- **Rule:** `isVisible()` does not wait. Click `#pending-tab`, then `waitFor` the card a few seconds before falling back to `#completed-tab`. The same holds for a dialog: an `isVisible({ timeout })` check missed the END ENCOUNTER? modal, leaving the visit open. `waitFor` the button you must press.

## A check over an empty collection passes

- **Symptom:** "no Next Steps card" passed, but the `.card` list it searched was empty — the encounter page draws no `.card` at all.
- **Wrong conclusion:** the activity is absent.
- **Rule:** before asserting that something is absent from a list, assert the list is not empty, or read a counter the page prints ("To Do (0)"). An absence proven by a selector that matches nothing proves nothing.

## A server-side edit of a measurement skips what the app does after a save

- **Symptom:** a test record changed with drush looked right, but the Case Management results record still named the test.
- **Wrong conclusion:** the PR leaves the entry open.
- **Rule:** saving a lab test in the app also rewrites the encounter's results record (`generate…LabsTestAddedMsgs`); a drush edit does not. A server-built fixture stands only for the record edited — say which follow-on records it leaves stale, and do not judge them.

## Reading a Dashboard figure before its data loads

- **Symptom:** straight after opening the CHW Dashboard, Case Management "Home Visit" read 0; a minute earlier it read 3.
- **Wrong conclusion:** the figure changed, or the baseline is 0.
- **Rule:** `page.goto('/')` reloads the app, and the cards read 0 until follow-ups load. Poll the value until it holds for a few seconds before recording it.

## `backdateEncounter` also ends the visit

- **Symptom:** a scenario needs a visit started yesterday and still open; the helper's backdate would have left it ended.
- **Wrong conclusion:** the helper moves only the start date.
- **Rule:** `h.common.backdateEncounter` sets both `value` and `value2` of `field_scheduled_date`, and `value2` is the end date (null = open). To keep a visit open, set only `value` in your own drush script.

## Reloading as a CHW asks for the village again

- **Symptom:** after `page.goto('/')` as CHW, waiting for `.wrap-cards` times out on a "VILLAGE" screen.
- **Wrong conclusion:** the sign-in was lost or the app hung.
- **Rule:** a full load makes a CHW pick the village again. Click "Akanduga at Mbirima" if it shows, or move by in-app navigation. The nurse's health centre choice survives a load. `h.acuteIllness.navigateToParticipantPage` calls `page.goto('/')` when the main menu is not showing, so it runs into this too (a dead 10 s video on PR #2457).

## Playwright's `page.clock` stalls the app

- **Symptom:** with `page.clock.setSystemTime(...)`, a new nutrition visit's page showed a spinner for over a minute.
- **Wrong conclusion:** the app cannot load a visit created on that date.
- **Rule:** `page.clock` fakes the page's timers too, and once installed it stays until the driver restarts. To change the date, shift only `Date` (recipe in app-map, The QA driver).

## A shifted page date leaves the service worker on the real date

- **Symptom:** on 1 October (shifted), a new antenatal and an acute illness follow-up were missing from the dashboard and from Case Management until the page was reloaded.
- **Wrong conclusion:** the app does not show new follow-ups straight away. At the real date it does.
- **Rule:** with a shifted date, reload before reading any count, and say so in the report. Recheck anything odd at the real date before calling it a bug.

## The participant page's back arrow goes to the search page

- **Symptom:** a script waiting for the main menu after one back click timed out on "SEARCH EXISTING PARTICIPANTS".
- **Wrong conclusion:** navigation broke.
- **Rule:** from a participant page, press back until `.wrap-cards` shows, or reload to `/`.

## A fixture that never reaches the defect passes on both builds

- **Symptom:** the nurse's OPV form for a child followed by both programs showed doses 1, 2 in order and the right date range, with the PR.
- **Wrong conclusion:** the fix is verified. The same screen on develop showed the same: with dose 1 at Child Scorecard and dose 2 at Well Child, develop's merge happened to come out in order.
- **Rule:** for an ordering or merge fix, open the screen under test on develop first, with the same data. Swap the overlaid files back, wait for gulp, then apply the update in the driver. Only a fixture where develop shows the defect is evidence; keep the others as regression rows.

## A scripted form flow that forgets a follow-up question

- **Symptom:** after recording dose 2 "given previously", the script pressed Save, and the click failed on a disabled button.
- **Wrong conclusion:** the save broke.
- **Rule:** an Immunisation form asks again after each recorded dose while another is due. Work out from the dates which questions follow, and script each answer.

## `qa.form()` calls a Save button disabled when it has neither class

- **Symptom:** on prenatal Next Steps › Next ANC Visit, `qa.form()` reported `save: "disabled"`, while the screenshot showed a bright, pressable SAVE.
- **Wrong conclusion:** the button is off, so its Save "doing nothing" is expected.
- **Rule:** `qa.form()` reads `active` as enabled and anything else as disabled. `viewEncounterActionButton` sets neither class when allowed. Check for the `disabled` class, and look at the screenshot.

## Opening a To Do activity after coming back from the Completed tab

- **Symptom:** `completePregnancyDating` failed waiting for `.icon-task-pregnancy-dating`, though Pregnancy Dating was still to do.
- **Wrong conclusion:** the PR removed the activity, or dating was already saved.
- **Rule:** the encounter page keeps the tab it last showed. After opening an activity from Completed and pressing back, it is still on Completed. Click `#pending-tab` before a helper opens a To Do card.

## Chromium hands back a cached location fix

- **Symptom:** after `setGeolocation` to a new position, the registration form still showed the previous one.
- **Wrong conclusion:** the app kept a stale reading in memory.
- **Rule:** Chromium returns its last fix while it is younger than the request's `maximumAge` (60 s on the PR, 10 min before). Wait past that age, or restart the driver, between positions. Your own `getCurrentPosition` probe also refreshes the cache.

## `context.setOffline(true)` does not stop the app's sync

- **Symptom:** persons registered "offline" were on the server minutes later, and their downloaded copies replaced the tablet's.
- **Wrong conclusion:** the tablet stayed offline, so local-only state would last.
- **Rule:** the sync still ran. Do not plan a scenario on keeping a record local; treat any record as downloaded within about a minute of upload.

## `page.reload()` keeps the page you were on

- **Symptom:** after a sync check on Device Status, `page.reload()` then waiting for `.wrap-cards` timed out after 60 s, and the dead minute landed on the recording.
- **Wrong conclusion:** the app failed to load after the reload.
- **Rule:** a reload keeps the hash route. To reload onto the main menu, `page.goto('/')` first, then `page.reload()`.

## Reading button text with `innerText`

- **Symptom:** a check on the location picker's buttons expected "Nyange Health Center" and got "NYANGE HEALTH CENTER".
- **Wrong conclusion:** the picker shows a different name.
- **Rule:** `innerText` returns text as CSS draws it, and the app's buttons are uppercased by CSS. Read labels with `textContent` (`allTextContents()`) to compare with the data.

## `$` in a grep pattern for an Elm name is an anchor

- **Symptom:** `grep -c "Pages\$NCD\$Utils\$labsResultsWithTestAdded" Main.js` printed 0, although develop's bundle holds that function.
- **Wrong conclusion:** the bundle is mangled, or gulp serves a different build.
- **Rule:** Elm's compiled names contain `$`, which grep reads as end of line. Search them with `grep -F` (fixed string).

## Holding the download across a save that does not diagnose

- **Symptom:** a develop control held the download across the CHW Vitals save and showed "no popup, stored none" — but without a hold the diagnosis only arrives at the Acute Findings save.
- **Wrong conclusion:** develop reproduces the defect.
- **Rule:** before timing anything around a save, run the flow once and note which save brings the popup. Hold across that one; e2e comments name the activity, not the tab.

## A main tree whose develop is far behind origin

- **Symptom:** local `develop` was 110 commits behind `origin/develop`, and the PR touched files that origin had also changed.
- **Wrong conclusion:** overlaying only the PR's files gives the PR's build.
- **Rule:** compare `git diff --name-only HEAD origin/develop -- client/`. If it is not empty, overlay those files from `origin/develop` first (also the develop control), then the PR's. Restore all of them after.

## Dismissing a modal, then a helper that checks for it

- **Symptom:** after my own Continue click, `completePriorTreatment` failed hovering the same modal's Continue.
- **Wrong conclusion:** a second popup appeared.
- **Rule:** the modal fades out while `isVisible` still says yes. After dismissing, `waitFor({ state: 'hidden' })` before calling a helper that dismisses popups itself.

## A local-time `Date` handed to the date picker

- **Symptom:** an ultrasound entered as 3 October was stored as 2 October, and the EDD moved by a day.
- **Wrong conclusion:** the form or the encounter's date shifted it.
- **Rule:** driver scripts run in Node on the host's time zone, while the page runs in UTC. `new Date(2026, 9, 3)` is 2 October in UTC. Build dates as `new Date('2026-10-03T12:00:00Z')` or `Date.now() - n * 864e5`.

## A fixture "at another health centre" registered at Nyange

- **Symptom:** women registered while signed in at Muhondo carried `hc: 4` (Nyange) in their records.
- **Wrong conclusion:** the record reads the wrong shard.
- **Rule:** `registerAdult` always picks Nyange as Registering Health Center. For another centre, re-shard on the server (app-map, Dashboards) or fill the select by hand.

## Moving a visit across a birthday

- **Symptom:** a woman registered "aged 41" had no `age` risk after her visit was moved back a day.
- **Wrong conclusion:** the age rule is off by one.
- **Rule:** `registerAdult` sets the birth date to today minus N years, so moving any visit earlier makes her N−1. Register one year older than the age wanted at the moved visit.

## Recording admin pages with the Chrome extension

- **Symptom:** a GIF of four Time filter states showed two; zoomed and scaled screenshots were not captured.
- **Wrong conclusion:** the GIF is the evidence for every state checked.
- **Rule:** check values in Chrome if asked, but record admin pages with the QA driver. Its browser opens them after a `drush uli` link (app-map, Dashboards).

## Evidence below the fold of a recorded form

- **Symptom:** checks on a Lab Results tab passed, but the video's frame showed only the tab's header and date; the answer under test sat below the 1024 px viewport.
- **Wrong conclusion:** the recording is the evidence for what the checks read.
- **Rule:** the activity pages scroll an inner container, so `scrollIntoView` does not move them. After opening the tab, `page.mouse.move(400, 900)` then `page.mouse.wheel(0, 900)`. Check one frame per test before the next row changes its data.

## Changing a fixture before its recordings are checked

- **Symptom:** a server edit for a later row changed the record an earlier row's video showed. Re-recording that row meant undoing the edit, syncing, recording, and editing again.
- **Wrong conclusion:** a row is done once its checks pass.
- **Rule:** a row is done when its frame shows the evidence. Check frames before any step that changes the shared fixture.

## Restoring the main tree after a develop overlay

- **Symptom:** `git checkout HEAD -- <overlaid files>` failed on one path and restored nothing.
- **Wrong conclusion:** the tree is back on `develop`.
- **Rule:** a file new on `origin/develop` does not exist in HEAD, and one bad path aborts the whole checkout. Remove those files, then check the rest out, and confirm `git status` lists only the user's own changes.

## Taking a fixed id from memory into a check

- **Symptom:** a cold-worker registration "failed" its shard check: expected Nyange `5f89c5f3-…`, got `725a9a6f-…`.
- **Wrong conclusion:** the save went to the wrong health centre.
- **Rule:** `725a9a6f-…` was Nyange; the uuid in app-map was stale. Before checking a stored id, read it from the server (`field_uuid` of the nid) in the same run. A failed check that created a record means a re-run under a new name.

## Clicking an activity card straight after the save that returned to the encounter

- **Symptom:** a loop reopening Pregnancy Dating from Completed timed out on the card's click in its second round. The same click worked a second later.
- **Wrong conclusion:** the activity left Completed, or the save broke the page.
- **Rule:** after `saveActivity`, wait about a second, click the tab you want (`#completed-tab`) explicitly, then the card.

## Enabling a feature by a flag name that nothing reads

- **Symptom:** `drush vget hedley_admin_feature` listed `stock_management_enabled: 1`, so the flag looked on.
- **Wrong conclusion:** Stock Management is enabled; a missing card would be a bug.
- **Rule:** an old variable can outlive a rename. Check the name against `hedley_admin_get_available_features()`
  before trusting a flag, and set the name listed there.

## Judging a race from the final state of one develop run

- **Symptom:** a develop control for "the second save overwrites the first" ended with the record correct, so develop looked fine.
- **Wrong conclusion:** the defect does not reproduce locally. The two saves did disagree; arrival order decided which one stood, and that run happened to land the correct one last.
- **Rule:** when a fix is about two saves of one record, read each save, not the final state: the server's `node_revision` rows (with the `field_revision_*` tables) or the upload rows in `shardChanges`.

## Reading the upload queue after a fast sync took it

- **Symptom:** a check on the `shardChanges` rows added by a save found none, three seconds after the save.
- **Wrong conclusion:** the save queued nothing.
- **Rule:** with `syncSpeed` idle at 5 s, rows upload and leave the queue within seconds. Read the server revisions instead, or check the queue in the same command, straight after the save.

## Picking a lab test whose result brings Next Steps

- **Symptom:** a control meant to end the recurrent phase on the last lab result stopped on the encounter page with Next Steps to do.
- **Wrong conclusion:** the phase-end save does not happen on develop.
- **Rule:** an HIV result at the recurrent phase brings HIV health education into Next Steps, so the phase ends on that save, not on the result. Read `expectNextStepsTask` before choosing the tests. Syphilis, Hepatitis B and Hemoglobin (12) results bring none.

## Assuming the recurrent vitals recheck is dropped on a later day

- **Symptom:** a check that Examination is not offered the day after the first visit failed.
- **Wrong conclusion:** the PR offers it wrongly. The recheck is expected whenever the first-visit BP was marginal, whatever the day.
- **Rule:** take a screen's expectations from its `expect…` function, not from a nearby rule on the labs entry (`labsResultsTestData` is the one that looks at the day).

## Posting attached videos next to text

- **Symptom:** `gh pr comment --attach` (2.103) turned each `![](file.mp4)` into a `[name.mp4](url)` link under the caption line, and the comment held no `<video>`.
- **Wrong conclusion:** the upload failed, or videos cannot be posted.
- **Rule:** after posting, count `<video` in the comment's `body_html`. A player needs the bare URL alone in its own paragraph: rewrite each link to a blank line, the URL, a blank line, then PATCH the comment.

## Setting a Drupal date field through the wrapper with a timestamp

- **Symptom:** `$w->field_date_concluded->set(strtotime('+35 days'))` stored `2026-12-06 02:00:00`, not midnight like the app's saves.
- **Wrong conclusion:** the server edit stored the date the tablet will read as given.
- **Rule:** the wrapper converts the timestamp with the site's time zone. For a fixture, set the raw value (`$node->field_date_concluded[LANGUAGE_NONE][0]['value'] = gmdate('Y-m-d 00:00:00', …)`, `node_save`) and read it back.

## A server edit of a labs entry before the tablet's save reached the server

- **Symptom:** none here, but the tablet's queued save of the entry would have replaced the edit on upload.
- **Wrong conclusion:** the entry on the server is the one the tablet just saved.
- **Rule:** before editing an entry on the server, poll it until it shows the tablet's last save (its completed tests or date), then edit, then wait for the tablet's `shards` row to show the edit.

## A helper that skips a tab it was given no value for

- **Symptom:** `completeNutritionAssessment(page, { height, weight })` for a 3-year-old returned on the MUAC tab, Save off, the activity still To Do.
- **Wrong conclusion:** the helper finished the activity.
- **Rule:** the helper fills only the tabs it has options for. Read the reply's tabs, and pass every value the child's age brings (MUAC from 6 months; head circumference under 36 months, for a CHW visit too). Falling in again cost two continuation videos on PR #2455.

## Drafting the PR report with the Write tool in a subagent

- **Symptom:** Write refused the drafted `report.md` as a "report file" in a watch-mode subagent.
- **Wrong conclusion:** the report cannot be posted from a subagent.
- **Rule:** write the post script's input with a Bash heredoc in the scratchpad; it is tool input, not a report file.

## Expecting develop's defect exactly as the issue words it

- **Symptom:** the issue said a rejected character "stays in the box"; on develop the box dropped it and kept the digits after it (`724/50-17` became `7245017`).
- **Wrong conclusion:** the control does not reproduce the defect, or the build is wrong.
- **Rule:** an issue's account of the symptom can be off in detail. Record what develop actually shows; the control proves the defect when the searched ID cannot be found, whatever the box shows.

## Sharing a scratchpad folder with an earlier run

- **Symptom:** the scratchpad's `qa/` folder already held another PR's `report.md`, `recordings.md` and scripts.
- **Wrong conclusion:** the files there belong to this run.
- **Rule:** keep each run's scripts and drafts in `scratchpad/qa<pr>/`, so a post never picks up another PR's report.

## Pairing videos with captions by hand

- **Symptom:** the drafted recordings comment put the upload video under "test 4", whose video was the other-charts comparison.
- **Wrong conclusion:** the captions match the files because the names look alike.
- **Rule:** list the files the draft attaches (`grep -o 'qa-<pr>-[a-z0-9-]*\.mp4'`), numbered, and read each against its caption before posting.

## Showing a downloaded file by leaving the page

- **Symptom:** a CSV opened with `page.goto('file://…')` started a second download instead; shown with `page.setContent`, it flashed for a millisecond in the video, out of order.
- **Wrong conclusion:** the video shows the file's contents.
- **Rule:** keep the same document while recording. Put the file's text in a fixed `div` overlay with `page.evaluate`, scroll that, then remove it.

## Checking a report the moment it is chosen

- **Symptom:** a check that Demographics "still loads" at a health-centre scope failed: the page showed only the date inputs.
- **Wrong conclusion:** the PR broke the other reports at that scope.
- **Rule:** most Statistical Queries reports render only after a date range is set (`h.reports.setDateRange`); Nutrition needs none. Read the page before asserting on it.

## A probe that runs the whole scenario spends the fixture

- **Symptom:** a script meant to check that a Case Management entry was listed also pressed YES, unrecorded. It created the encounter the develop control was meant to show, and the entry vanished.
- **Wrong conclusion:** the probe changed nothing, so the recorded run could follow.
- **Rule:** a probe stops before the first action that writes. Undoing one costs server edits: mark the stray encounter `field_deleted` 1, and clear the follow-up's `field_date_concluded`, which YES sets.

## A shell loop of recordings that carries on after a failed setup

- **Symptom:** a navigation step failed for the first child, and the loop still recorded both children's scenarios, producing two dead 10-second videos.
- **Wrong conclusion:** each iteration checks its own result.
- **Rule:** end each setup command in a loop with `|| break`, and check its `ok` before starting the recording.

## Building a fixture on a build that would mishandle it

- **Symptom:** on develop, opening a child's open CHW visit through the participant page would have created a new visit, because the fixture's second participant was the one develop picks.
- **Wrong conclusion:** fixture steps run the same on any build.
- **Rule:** finish every fixture step that goes through the screen under test before adding the server-side data that triggers the defect. If that order is impossible, hide the data for a while (`field_deleted` 1, sync), then restore it.

## Waiting on gulp with `pgrep -f` on the compiler's command line

- **Symptom:** a Monitor waiting until `pgrep -f "elm make …/Main.elm"` found nothing never ended, though Main.js had long been rebuilt. A `pkill -f` to stop those monitors then killed its own shell (exit 144).
- **Wrong conclusion:** gulp was still compiling.
- **Rule:** `pgrep -f` / `pkill -f` match the waiting script's own command line. Look for the compiler as `ps -eo pid,cmd | grep "[/]usr/bin/elm make"`, and judge the build by a name in `client/serve/Main.js` (`grep -cF`).

## Encounter-reading helpers that leave the Completed tab open

- **Symptom:** after a scripted read of both encounter tabs, `completeDangerSigns` failed waiting for `.icon-task-danger-signs`; a second take of the preparation video was needed.
- **Wrong conclusion:** Danger Signs was not offered.
- **Rule:** the same pit as "Opening a To Do activity after coming back from the Completed tab", met again by a reader script. Any script that clicks `#completed-tab` ends by clicking `#pending-tab`.

## A screen that looks the same with no data at all

- **Symptom:** on a re-paired device, the participant page of a woman whose pregnancy had ended showed First Antenatal and Postpartum on, Record Pregnancy Outcome off.
- **Wrong conclusion:** the concluded pregnancy came down. A woman with no pregnancy on the device gets exactly the same buttons.
- **Rule:** when the expected screen equals the empty one, prove the download another way: read the record from IndexedDB `shards` on the new device.

## Timing a sync resend from the server's side

- **Symptom:** a held download's copy reached the server 20 s after the page sent it, and an "extra" request in the window turned out to be the newer request's own 30-second resend.
- **Wrong conclusion:** the build under test sends a request it should not.
- **Rule:** Chrome holds a second GET to the same address for up to 20 s while the first is open, and every copy of a download has the same address. The app's clock moves every 10 s. Time scenarios from the page's own request log, and keep windows short of the next resend.

## Waiting on a log line that existed before the step

- **Symptom:** "wait for the copy to answer" matched an authority reply from before the hold was set, so the next hold caught the wrong request.
- **Wrong conclusion:** the scenario ran as planned.
- **Rule:** when waiting for an event in a log, require it to come after the step that should cause it (compare timestamps or ids).

## Taking "the week 20 line" as one comparison

- **Symptom:** a diabetes control entered a sugar result at 20 w 1 d for a 19 w 5 d visit, and develop stored `diabetes`, the same as the PR would.
- **Wrong conclusion:** develop does not show the defect for diabetes.
- **Rule:** the lines differ: hypertension splits at `< 20` / `>= 20`, diabetes at `<= 20` / `> 20`. Read each rule's comparison and put the entry day past it (21 w 1 d). A shifted clock can place the visit itself on any day.

## Building a fixture's dates from Node while the page clock is shifted

- **Symptom:** none here; a visit created at a +3-day page offset would have read 3 days older than planned.
- **Wrong conclusion:** `Date.now() - n days` in a driver script is the page's today.
- **Rule:** driver scripts run on the real clock. With `qaDateOffset` set, add the offset (read it from the page's localStorage) to any date a form is given.

## Pairing videos with captions by hand (met again)

- **Symptom:** two preparation captions in the drafted recordings comment named each other's videos.
- **Wrong conclusion:** none: the numbered listing of attached files caught it before posting.
- **Rule:** keep running that listing; when develop and PR fixtures share a name pattern, the numbers in the file names do not follow the caption order.

## Planning a reproduction from the entry's mechanism alone

- **Symptom:** a B-548 scenario built on "only the first pending child is loaded" saw the correct behaviour: tapping a child loads that child's data through `Pages/Activity/Update.elm` (`SetSelectedParticipant`), which the entry never read.
- **Wrong conclusion:** the reproduction failed, so the defect is not real. A narrower case (child shown without a tap) did reproduce.
- **Rule:** before planning, read every path that can load the data — the page's `Fetch.elm` and the out-messages of its `Update.elm` — and aim the defect scenario at the case none of them covers.

## One long scripted command that writes data and reports only at its end

- **Symptom:** a develop oracle run saved four group weights, then failed on its last step; the reply held no values, and the fixture could not run again.
- **Wrong conclusion:** a failure costs only a re-run.
- **Rule:** a script that writes returns nothing if a later step fails. `qa.log` each step's values as soon as it reads them, or end the command before the next write.

## Taking a tap as proof the child's data is loaded

- **Symptom:** on develop, a tapped child's request log showed the child's fetch, so the child looked loaded; its Nutrition save then gave no assessment and Weight showed no previous value.
- **Wrong conclusion:** the tapped child is loaded, as the issue and the backlog entry said.
- **Rule:** the tap's fetch is one-shot and loads one level. Judge loading by a screen it changes (Weight's "Previous measurement"), not by a request.

## A global set in an earlier driver command

- **Symptom:** a new request listener never recorded anything; an older `globalThis.__reqlog` from an exploring command already existed, so the guard skipped installing it.
- **Wrong conclusion:** the page made no such requests.
- **Rule:** globals live as long as the driver. Give a new listener a new global name, or remove the old one.

## Reading the group assessment from the `nutrition` node

- **Symptom:** after popups showed "Moderately underweight", every group `nutrition` node's `field_nutrition_assesment` was empty on both builds.
- **Wrong conclusion:** the save dropped the assessment, so the PR broke it.
- **Rule:** the group engine updates that node only when the assessment changes with the save. Read the `follow_up` node, and compare with develop on a loaded child before blaming the change.

## Taking the encounter id from the end of an activity URL

- **Symptom:** after a Nutrition save, the script's read of the encounter's stored indicator returned null, and the control needed a second command.
- **Wrong conclusion:** the save stored no indicator.
- **Rule:** an activity page's route is `#prenatal-activity/<uuid>/examination`, so `split('/').pop()` gives `examination`. Take the id with a uuid regex.

## Running the skill from a main tree behind origin

- **Symptom:** the main tree's `develop` was five commits behind `origin/develop`, and two of them changed this skill (hold-row rules, the report's first line).
- **Wrong conclusion:** the SKILL.md loaded at the start is the current one.
- **Rule:** at Step 2, `git diff HEAD origin/develop -- .claude/skills/qa-tester/` and follow any newer rules for the run.

## A server-built person the tablet cannot decode jams the whole health centre

- **Symptom:** after `h.familyNutrition.addChildViaDrush`, `syncAndWait` timed out with Nyange on "Status: Downloading", Remaining 0 / 0. The same download went out every 30 s and got a 200 back.
- **Wrong conclusion:** the server is slow, or the PR broke sync.
- **Rule:** read Device Status › Error log first. It said `mode_of_delivery` "svd is not a recognized ModeOfDelivery": the helper writes `svd`, a value the client does not know. Every device syncing that centre stalls until the node is fixed (set `svd-no-episiotomy`, `node_save`). After any server-built record, sync once and check the Error log before going on.

## Un-picking the last NCDA ANC month answers the question No

- **Symptom:** a boundary script picked month 1, un-picked it, and the next check found no month grid at all; the answer read No.
- **Wrong conclusion:** the tap missed and hit "No", or the PR broke the grid.
- **Rule:** every NCDA host's `ToggleANCVisitDate` sets the answer to No when the picked set becomes empty. Keep one month picked while testing pick and un-pick of others.

## Tagging form inputs with ids, then clicking them after a re-render

- **Symptom:** a generic NCDA filler collected every unanswered yes/no, set an id on each, and its second click waited 10 s for an id that was gone.
- **Wrong conclusion:** the question disappeared.
- **Rule:** an answer can reveal a question and Elm rebuilds the nodes, dropping ids it did not set. Answer one question, then scan the form again.

## Looping over a fixed number of growth charts

- **Symptom:** a recorded loop over charts 1–4 timed out for a 6½-year-old, leaving a dead 10 s tail; the child had to be registered again and re-recorded.
- **Wrong conclusion:** the report failed to draw a chart.
- **Rule:** the chart count depends on age: four under 60 months, three after (app-map). Loop over the `svg` elements that hold `line.st18`, not a fixed list of indexes.

## Planning a Burundi row in an unattended run

- **Symptom:** `ddev drush vset hedley_general_site_name burundi` was refused as a change to a shared resource, and setting `site` in the test browser's `syncInfoGeneral` (directly, then from an init script) still read `rwanda` after the reload.
- **Wrong conclusion:** a device-only edit stands in for the server setting.
- **Rule:** the running app writes `syncInfoGeneral` back, so the site follows the server. A watch run that needs Burundi reports the row as blocked, unless whoever started it already set the server variable, with approval (`drush vget` reads `burundi`, and the device's `syncInfoGeneral.site` confirms it after a fresh pairing). Do not fake the sync reply to get past it.

## An attachment upload that fails part-way leaves a posted comment

- **Symptom:** `gh pr comment --attach` ended with HTTP 408 on one video, yet the comment existed, with one player and raw `client/qa-recordings/…` paths for the rest.
- **Wrong conclusion:** a failed command posted nothing.
- **Rule:** after a failure, read the PR's comments. Post again in full, check every video has its `<video`, then delete only the partial comment your own command made.

## Installing a page hook before the fixture finishes

- **Symptom:** a request-watching hook set with `page.evaluate` read as undefined a step later: `Cannot set properties of undefined`.
- **Wrong conclusion:** the evaluate call failed, or the app removed the global.
- **Rule:** registering a family reloads the page (the attendance and relationship pages navigate away), and a reload wipes `window`. Install page hooks after the last fixture step, right before the scenario.

## Repeating a tap inside a multi-step form

- **Symptom:** the group NCDA has six steps, and repeating every Save tap would have advanced two steps at once and skipped a step.
- **Wrong conclusion:** the double-tap scenario cannot be run on that form.
- **Rule:** repeat the tap when the save's request leaves, not on every click: in the `XMLHttpRequest.send` wrapper, on the POST of the type under test, click the last clicked button once.

## Waiting for a download at the default sync speed

- **Symptom:** a server edit did not change an open page in over three minutes, against the backlog entry's prediction.
- **Wrong conclusion:** a download does not cause the defect.
- **Rule:** the default loop runs every 10 minutes. Set `syncSpeed` idle 3000 first, and check the device's `shards` row and `lastFetchedRevisionId` before reading the result.

## Expecting a diagnosis's name to show every part of its meaning

- **Symptom:** a check that a positive-and-pregnant follow-up's popup names pregnancy failed, on the right build with the right data.
- **Wrong conclusion:** the PR shows the wrong diagnosis.
- **Rule:** the English label of `DiagnosisMalariaUncomplicatedAndPregnant` is the same as plain uncomplicated malaria. The stored `acute_illness_diagnosis` tells them apart; read `Translate.elm` before writing a check on a label, and prefer the stored value.

## Assuming a child's first visit goes the way an adult's does

- **Symptom:** `completePriorTreatment` timed out waiting for the encounter page on a nurse's 5-month-old with a cold.
- **Wrong conclusion:** the helper is broken, or the page did not save.
- **Rule:** when the diagnosis brings a next step, the save that completes it opens Next Steps behind the popup, before Prior Treatment is done. Do Next Steps first and read `Pages/AcuteIllness/Activity/Utils.elm`'s first-encounter order before chaining helpers.

## A substring that also matches longer names, and a name below the fold

- **Symptom:** `.follow-up-entry` picked with "Qa2493P1" matched P10–P12 in a list of a dozen entries, and the entries asked about sat below the 1024 px view of the video.
- **Wrong conclusion:** the recording shows the entries it reads.
- **Rule:** match with a regex that ends the name (`/Qa2493P1(?!\d)/`) and scroll the list before the frame you rely on.

## A long chain of driver commands in one Bash call

- **Symptom:** a shell call chaining three recorded controls and two capture loops returned "moved to the background after 180s", with its output cut off.
- **Wrong conclusion:** the driver hung, or the rest of the chain did not run. It kept running.
- **Rule:** keep each Bash call under three minutes, or write every result to a file (`> res-<name>.json`) and read those files; never rely on the printed output of a long chain.

## Ending a recording on a long Case Management list

- **Symptom:** every recorded row ended with the filter just pressed, and the checked name sat far below the 1024 px view in the Home Visit list (16 QA entries in front of it).
- **Wrong conclusion:** a passing `qa.check` makes the video the evidence for that row. It showed only other people's entries.
- **Rule:** finish a recorded list check by scrolling the entry into view and holding a second or two, and look at the last frame before the next fixture. Case Management lists keep every past run's entries, so the row under test is rarely above the fold.

## Restarting the driver fresh before the develop fixtures uploaded

- **Symptom:** a develop control (37 weeks, dipstick read at the visit) was missing from the server's diagnoses query; the other controls were there.
- **Wrong conclusion:** the save failed, or develop drops the encounter.
- **Rule:** `qa.sh start --fresh` throws the old profile away, queued uploads included. Before restarting to load the PR build, `syncAndWait` and confirm Remaining for Upload: 0.

## A Next Steps page that opens behind a diagnosis popup

- **Symptom:** `qa.click('.icon-back')` on prenatal Next Steps timed out: "`<div class="overlay">` intercepts pointer events".
- **Wrong conclusion:** the page hung, or the save broke.
- **Rule:** prenatal Next Steps (visit and Lab Results) opens behind a "Patient shows signs of …" popup whenever a diagnosis is made, emergency or not. Press its CONTINUE before any other click.

## Taking "the last row of a type" as the fixture's record

- **Symptom:** a probe of the newest `core_physical_exam` and `vitals` rows showed a normal abdomen and 120/80, against a fixture saved with tenderness and 165/115.
- **Wrong conclusion:** the form did not save what was entered.
- **Rule:** `shards` holds every Nyange patient, in no useful order. Find the person by label, then filter rows by `person`.

## Filling the dipstick result by its option labels

- **Symptom:** a filler that chose "+1" in every select holding a "+1" option also set glucose to +1.
- **Wrong conclusion:** none here; the diagnoses happened not to depend on it, but the control did not match the PR fixture.
- **Rule:** prenatal Urine Dipstick protein and glucose selects have the same options (0, +1 … +4). Pick each select by its position (protein, pH, glucose) or its label.

## Reading the service worker's cache by a bare path

- **Symptom:** a check that the browser runs the PR build read `caches.match('/Main.js')` and got "no cache entry".
- **Wrong conclusion:** the page is not running a cached build, or the overlay never reached the browser.
- **Rule:** the precache key carries `?__WB_REVISION__=<hash>`. Walk every cache's keys for `Main.js` and test that body for the overlay's name.

## Waiting on a result file inside one long Bash call (met again)

- **Symptom:** a `timeout 600 … until [ -s file ]` wait was moved to the background after 180 s, its summary lost.
- **Wrong conclusion:** none here, but the same pit as "A long chain of driver commands in one Bash call".
- **Rule:** cap each wait at about 175 s and repeat it, or rely on the background task's completion notice.

## Expecting both immunisation answers to stay on screen after "will receive today? Yes"

- **Symptom:** a develop control's check that DTP showed "prior to today? No" and "today? Yes" failed: only the today question was left.
- **Wrong conclusion:** develop dropped the first answer.
- **Rule:** the form hides "prior to today?" once "will receive today?" is Yes. Read the form builder's conditions before scripting a check; also, `qa.form().save` reads `enabled` / `disabled`, not `active`.

## A compound shell loop refused by the safety check

- **Symptom:** one Bash call that registered a fixture and ran three recorded tests in a `for` loop with nested `$( … )` was refused as an unreadable `rm` command, though it held no `rm`.
- **Wrong conclusion:** the command ran, or a workaround is needed.
- **Rule:** keep driver calls plain: write small runner scripts (`run.sh <name> <prelude> <files…>`) into the scratchpad once, then call them one test per Bash call.

## Picking a Next Steps tab by its icon

- **Symptom:** a develop control's reader found no Medication Distribution tab after the recheck, so the helper saved the form before its text was read.
- **Wrong conclusion:** develop offers no medication there. It offered Methyldopa; reopening the saved form from Completed showed it.
- **Rule:** the recurrent Medication Distribution tab carries no `icon-next-steps-treatment`. Pick Next Steps tabs by label text, and read a form before the step that saves it.

## Starting a waiter on a background driver run

- **Symptom:** while a recorded visit ran in the background, two `until … grep` waiters each hit the Bash limit and became background tasks of their own.
- **Wrong conclusion:** waiting in a foreground loop is how to get the result.
- **Rule:** the same pit as "A long chain of driver commands in one Bash call". A run started with `run_in_background` sends its own notice; wait for it and start no waiter.

## Overlaying a stacked PR's files onto a develop that moved on

- **Symptom:** the PR's branch was cut before develop merged another PR that changed the same two Elm files.
- **Wrong conclusion:** checking the PR's files out over develop gives the PR's build, and develop is its control.
- **Rule:** build both trees with `git merge-tree --write-tree origin/develop origin/<branch>` (and the parent branch for a stacked PR), check the files out from those trees, and use develop plus the parent as the control.

## `endPrenatalEncounter` returning on a disabled End Encounter

- **Symptom:** the next day's Subsequent Antenatal button stayed disabled; the earlier visit's `field_scheduled_date` end was still NULL.
- **Wrong conclusion:** the helper ended the visit, so the later visit can start.
- **Rule:** the helper force-clicks End Encounter, so a disabled button passes silently. Check the stored end; end a fixture visit with `backdatePrenatalEncounter(name, days)` after upload.

## `completeTreatmentReview` answers Yes to missed doses and adverse events

- **Symptom:** on a later visit's Hypertension Medication tab, a hospital-referral question appeared and Save stayed off; the helper timed out.
- **Wrong conclusion:** Treatment Review is broken.
- **Rule:** the helper says Yes to every question. Answer `missed-doses` and `adverse-events` No by hand, and re-scan the form after each answer.

## Reading a checkbox's state from the wrapper class

- **Symptom:** a check that "Hypertension before pregnancy" was ticked failed, though the screen showed it ticked.
- **Wrong conclusion:** the click missed.
- **Rule:** Medical History's `.ui.checkbox` wrapper carries no `checked` class. Read `qa.state().choices` (`(x) …`).

## Giving `qa.sh record start` a name with the prefix

- **Symptom:** the video came out as `qa-2511-qa-2511-test1-….mp4`.
- **Wrong conclusion:** none; the file had to be renamed.
- **Rule:** the driver adds `qa-<pr>-` itself; pass only `test<n>-<name>`.

## A name regex that ends with `(?!\w)` on a Case Management entry

- **Symptom:** "Buhara TB pane lists X" failed, though the logged entry read "E2ETest Qa2515XThis monthTuberculosis Follow up".
- **Wrong conclusion:** the pane does not list X.
- **Rule:** an entry's `textContent` runs the name straight into the next cell ("…XThis month"). End a name match with `(?!\d)`, as "A substring that also matches longer names" says, not `(?!\w)`.

## A control script that assumes the role an earlier script left

- **Symptom:** the develop control for the CHW's Case Management requests ran as the nurse, and every pane differed from the PR's.
- **Wrong conclusion:** develop's panes differ from the PR's.
- **Rule:** the last PR script signed in the nurse, and the build update kept that session. Start every control script with its own `qa.signIn`, and read `session` in the reply before comparing.

## Opening the CHW Dashboard without server statistics

- **Symptom:** a recorded dashboard row waited 60 s on "CHW SNAPSHOT" with a spinner, leaving a dead video.
- **Wrong conclusion:** the dashboard is broken on this build.
- **Rule:** app-map already says it needs the health centre's statistics. Before recording, run `hedley_stats_calculate_stats_for_health_center(4)`, sync, and reload.

## Clicking a main-menu card by its text

- **Symptom:** `qa.click('PARTICIPANT DIRECTORY')` returned ok and the page stayed on the main menu.
- **Wrong conclusion:** the click navigated; the next step then waits on the wrong page.
- **Rule:** the card's handler is on its icon. Click `.icon-task-participant-directory` (as the helpers click `.icon-task-clinical`), and check the URL before going on.

## A form loop that picks "the first tab not completed"

- **Symptom:** the Medication script saved Fefol twice and MMS twice; the log showed the same tab answered again after its save.
- **Wrong conclusion:** the save did not take, or the app asks again.
- **Rule:** the active tab never carries `completed` (app-map, Controls), and after some saves the app leaves a saved tab active. Track the tabs the script has saved by name, not by class.

## `ddev` run from the scratchpad

- **Symptom:** `ddev mysql < file.sql` failed with `unknown command "mysql" for "ddev"`.
- **Wrong conclusion:** ddev or the database is down.
- **Rule:** ddev finds its project from the working directory. Prefix every ddev call with `cd /var/www/html/ihangane &&`.

## Logging in to Drupal a second time in one driver session

- **Symptom:** a second report recording failed at once: `input[name="name"]` never appeared on `/user/login`.
- **Wrong conclusion:** the admin site is broken.
- **Rule:** the driver keeps the Drupal session. Open `/user/login` and call `h.reports.drupalLogin` only when the name field is visible.

## Waiting for an element that was already showing before the click

- **Symptom:** on develop, a check that the banner came back after a second failed Save read 0, though the video showed it.
- **Wrong conclusion:** the second save did not fail, or the banner went for good.
- **Rule:** the stale banner was still there when `waitFor` ran, so the wait returned at once and the count fell in the Loading moment, when the banner is hidden. When the element can already be there before the action, wait for the change: pause past the request first, or wait for it to go away and then come back.

## Planning a row on the browser Back button

- **Symptom:** the first plan held a hold row for reaching the Relationship page with `page.goBack()`.
- **Wrong conclusion:** the Back route is a user path the change must not alter.
- **Rule:** the user closed B-544 because the tablets have no Back button. Read the backlog item behind a "residual" before planning a row on it, and reach pages only through the app's own arrows and buttons.

## A develop control run in the session that filled the form

- **Symptom:** on develop, a Treatment Review saved Yes / No / No and changed in the same session showed "Why not?", "How many doses?" and the adverse events unanswered, Save off, as the PR should.
- **Wrong conclusion:** develop does not have the defect. After a reload, develop showed 7/7 with Save on and stored `none` / 0 / `none`.
- **Rule:** a page keeps its form, dirty flags included, after a save. For a fix about reopening a saved record, reload the app before reopening, on both builds, as a nurse coming back later would.

## Finding the "How many doses?" select by `select.form-input`

- **Symptom:** `selectOption` on `select.form-input` timed out on the treatment review, leaving a dead 10 s tail and a re-recorded video. `qa.form().selects` was empty too.
- **Wrong conclusion:** the select was not shown.
- **Rule:** `viewCustomSelectListInput` draws a select without the `form-input` class. Use `.ui.form select` and read its `inputValue()`.

## Pressing a disabled Save inside `.actions` (met again)

- **Symptom:** a develop control ended with a 10 s hover timeout on "Save", which was disabled.
- **Wrong conclusion:** none; app-map already says `.actions` takes the pointer from a disabled button.
- **Rule:** before a scripted save, read the button's `active` class and save only when it is there.
