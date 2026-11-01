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
- **Rule:** a full load makes a CHW pick the village again. Click "Akanduga at Mbirima" if it shows, or move by in-app navigation. The nurse's health centre choice survives a load.

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
- **Rule:** the helper fills only the tabs it has options for. Read the reply's tabs, and pass every value the child's age brings (MUAC from 6 months; head circumference only under 36 months).

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
