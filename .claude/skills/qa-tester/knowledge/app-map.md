# QA App Map — E-Heza

What a tester who knows the app carries in their head and the e2e helpers do not: accounts,
which screen shows what, sync gates, and the parts of routes no helper drives. Correct this
file whenever reality disagrees with it, merging each fact into the section it belongs to.

How to fill a form and press on is in `client/e2e/helpers/`; selectors and per-encounter
mechanics are in `../../e2e-test/references/e2e-knowledge-base.md`. Neither is repeated here.

## Environment

- App URL `http://localhost:3000`, served by `ddev gulp` from the **main tree** — whatever
  branch that tree is on.
- `span.version` (top right) reads `Version: <short hash>`. Clicking it (`div.version-env`)
  opens the service-worker page, where a new build is applied: after gulp rebuilds, CHECK FOR
  UPDATES, then ACTIVATE NEW VERSION OF THE APP, then reload. Each browser profile keeps
  its own service-worker cache, so two browsers can show two builds at the same time —
  always read the version in the browser doing the run.
- `EHEZA_SITE` (rwanda / burundi) is set in `.ddev/config.local.yaml`.
- Drupal admin: `admin` / `admin`.
- GitHub CLI 2.99 or later is required: it is the first with `--attach` for videos, and
  older ones fail on this repo's `gh issue view` / `gh pr view` (Projects classic).
- Hedley scripts run as `ddev drush scr profiles/hedley/modules/custom/<module>/scripts/<file>.php`.
  Most take `--dry_run` and `--nid` (only records after that nid), so a run can be limited
  to the records it created.
- A measurement node is found by its patient: join `field_data_field_person` to the person
  node and match its title (`E2ETest <firstName>`).
- A **server reinstall** (`server/install`) wipes every device, person and pairing, restores
  the migration devices (Tablet 1 `12345678`, Tablet 2 `87654321`) and turns the feature
  flags back on. A browser profile from before it holds credentials for a database that no
  longer exists: start the driver with `--fresh`.
- Every upload failing, with the watchdog showing `Autoloader not found: …/sites/all/vendor/autoload.php`
  and `Class 'Ramsey\Uuid\Uuid' not found`, means the server's Composer libraries are missing. The
  user runs `composer install` (or `composer update <new package> --with-all-dependencies` when the
  lock lacks a package) in `server/www/sites/default/files/composer`, inside ddev.
- Re-pairing an already-paired device needs super user mode
  (`ddev drush vset hedley_super_user_mode 1`, back to 0 after).

## Feature flags

`ddev drush vset hedley_admin_feature_<name>_enabled 1|0`. The canonical list is
`hedley_admin_get_available_features()` in `hedley_admin.module`. A screen behind a disabled
flag simply does not appear.

## Accounts and roles

| account | PIN | after sign-in |
|---|---|---|
| Nurse Maya | 1234 | picks a health centre: Muhondo or **Nyange** (QA uses Nyange) |
| CHW Jojo | 2345 | picks a village: Akanduga at Mbirima, or Busake at Busake |
| Lab technician | 3333 | picks a health centre; the menu has only Case Management and Device Status |

- Switching account keeps the device paired: `qa.signIn(pin, location)` goes to the main
  menu, signs out, enters the PIN and picks the location — about a second.
- Individual encounters offered — nurse: Acute Illness, Antenatal Care, Child Nutrition,
  Noncommunicable Diseases, Standard Pediatric Visit. CHW: Acute Illness, Antenatal Care,
  Child Nutrition, Well Child Visit, Child Scorecard, TB Management, HIV Management.
- **The newborn exam (Birth History) is CHW-only.** A nurse always starts a `PediatricCare`
  encounter, which never offers it (`Pages/WellChild/Participant/View.elm` picks
  `NewbornExam` only for a CHW and a child under two months).
- A CHW's participant search lists only people living in the chosen village. A nurse's registration
  puts a woman in Birambo, so a woman both roles must open is registered by the CHW: Akanduga
  belongs to Nyange, where the nurse finds her.
- Group sessions: a CHW opens straight onto Attendance; a nurse goes Clinical → Group
  Encounter → programme → group.

## Device, sync and local data

- Pairing codes are single-use. `qa.freshDevice()` uses `88888888` and titles the device
  "QA Device …"; the e2e runs use `99999999` and delete every device titled "E2E…".
- The pairing lives in the service worker's `config` cache, not in localStorage, so only an
  empty profile (`qa.sh start --fresh`) can be paired anew; `qa.freshDevice()` refuses
  any other.
- After pairing, general data (nurses, villages, health centres) must download before a PIN
  works: "Your PIN code was not recognized" straight after pairing means not synced yet.
- **A health centre downloads nothing until its "START SYNCING" button on Device Status is
  pressed**; choosing it at sign-in does not start it. Until then Clinical shows "Data is not
  synced". `qa.freshDevice()` does this for Nyange.
- The upload lane then idles (Sync Settings → Idle time, default 600 s; after an error it
  sleeps that long). "TRY SYNCING WITH BACKEND" on Device Status forces a round;
  `h.common.syncAndWait(page)` presses the sync icon and waits for Success.
- Device Status shows each health centre in a `.health-center` block, with Last Successful
  Contact, Remaining for Upload, Remaining for Download and Status in `.sync-status`.
- "Status: Success" alone does not prove the upload: check "Remaining for Upload: 0" too.
  Queued changes are the IndexedDB `nodeChanges` and `shardChanges` rows.
- Device Status has "Error log" and "Sync Manager" debug panels behind its top-left links.
- IndexedDB `sync` stores: `nodes` (general data only, ~36 rows), `shards` (authority data:
  persons, relationships, measurements), `nodeChanges` / `shardChanges` (queued uploads),
  `authorityPhotoUploadChanges` (`{localId, photo, fileId}`, `fileId` null until the file
  uploads), `deferredPhotos`, `syncMetadata`, `statistics`.
- The upload is a main-thread XHR to `/api/sync`, so `page.on('request')` in the driver sees
  its body (`request.postData()`). That is the only way to tell a dropped key from a null
  one, since the backend treats both alike. Downloads go through the service worker.
- **Relationships load slowly after a fresh pairing.** A person page can show "Family
  Members: … Not Found" through several reloads before it starts working. It is a loading
  delay, not `clinics-not-found-bug.md`.

## Controls

- Main-menu cards: the click handler is on `div.card > div.image`, not the card or label.
- Clinical: `button.individual-assessment` / `button.group-assessment`.
- Participant search: type in `input.search-input`; each result is `.item.participant-view`.
- Search results and Participant Directory rows open through their `span.action-icon.forward`;
  the row itself is not clickable. The edit pencil (`span.action-icon.edit`) is on the person
  page, as is "Add Child" (`div.add-participant-icon-wrapper`).
- Save buttons: `button.ui.fluid.primary.button` with class `disabled` or `active`; the
  `disabled` attribute is never set, so read the class. Buttons from `viewEncounterActionButton`
  (prenatal Next Steps › Next ANC Visit) carry neither class when pressable — only `disabled` means off.
- Task tabs (`.link-section`): the active tab never carries `completed`, even when it is
  done (`not isActive && isCompleted`); read the task counter for the open tab instead.
- Yes/no inputs: the chosen side is `input.checked` inside `.form-input.yes-no.<name>`; the
  label never gets an `active` class.
- The out-of-range warning covers the page with a dimmer; its Close button is the only
  in-app way out, while browser Back goes around it and leaves the warning standing.
- End Encounter is `ui fluid primary button` + `disabled` until the mandatory activities are
  done, then `active`. Acute Illness then asks in an in-app "END ENCOUNTER?" modal; Well
  Child and Home Visit end without one. None of these are native dialogs.
- Date of birth: open `div.date-input.field`, set YEAR (it rebuilds the MONTH list), then
  MONTH, click the day, then the popup's SAVE — the day click alone does not close it.
  `h.common.setDate` does this.
- Photos: the widget is a Dropzone (`#dropzone`), not a camera. Give it a file with
  `setInputFiles` on its hidden input; the thumbnail turning into a `/cache-upload/images/<id>`
  URL means it took.

## Registration

- The nurse's form has the address cascade (Province → District → Sector → Cell → Village)
  and Registering Health Center; the CHW's form has no address section.
- Fields depend on the date of birth: "Mode of delivery" appears for a child, "Level of
  Education" / "Marital Status" for an adult. Set the date first.
- Address values: Amajyaruguru 2755 → Gakenke 2756 → Coko 2803 → Mbirima 2809 → Akanduga
  2810. Nyange HC uuid `5f89c5f3-34a2-5760-afc5-731db5c8cff9`.
- `registerAdult` names everyone `E2ETest <firstName>` (Drupal title "secondName firstName").
  A nurse's registration takes the first option at each address level: Amajyaruguru →
  Gakenke → Busengo → Birambo → Birambo.
- Demo data: 86 persons, all with a migration photo; demo children are born in 2022, too old
  for head circumference or the newborn exam — register a newborn for those.
- **Editing a demo person needs Level of Education and Marital Status**, which the migration
  left empty; the form refuses to save without them.
- An address edit propagates to children only if each child is loaded: open the child's own
  person page first, then go back and edit the parent.
- Saving a photo measurement also sets the **person's** photo, server-side
  (`HedleyRestful*Photos`, around line 104). Re-read a person's photo fid after any photo
  activity before asserting on it.

## Encounters: what the helpers do not tell you

- Prenatal Laboratory: Random Blood Sugar is the last tab; performed today Yes → Point of
  Care → before meal → the mg/dL input appears. NCD Laboratory tabs: Random Blood Sugar,
  Creatinine, Urine Dipstick, HIV, Liver Function, Lipid Panel, HBA1C. Its Random Blood Sugar
  asks performed (`test-performed`) → `immediate-result` (Point of Care / Lab) →
  `patient-fasted` → `test-performed-today`; Lab ends on "update test results via case
  management". Creatinine: performed → performed today. No reveals five why-not reasons.
- NCD Next Steps appears only after Danger Signs, Symptom Review, Examination and Medical
  History are saved, even when a lab result already diagnoses (the four helpers do it). Its
  Medication Distribution names the diagnosis: "This patient has Diabetes with glucose … of
  250 mg/dL". The encounter's diagnoses are `field_ncd_diagnoses` (`diabetes-recurrent`).
- Acute Illness laboratory (nurse, initial encounter): the COVID rapid test is offered only
  while there is fever with respiratory symptoms (or two or more general ones) **and the
  malaria RDT is not positive** (`covid19SuspectDiagnosed`). Turning malaria Positive takes
  the COVID tab away. "Currently pregnant?" follows a positive result, only for a woman of
  child-bearing age. Saving either test re-diagnoses, with an assessment dialog (CONTINUE).
- Acute Illness Prior Treatment: yes/no inputs `fever-past-6-hours`, `malaria-today`,
  `malaria-within-past-month`. A Yes reveals "Do you feel better after taking this?" as
  `<question>-helped`.
- A saved activity is reopened from the encounter's Completed tab (`#completed-tab`), then
  its `.icon-task-<activity>` card.
- NCD Medical History has five tabs: Co-Morbidities, Medication History, Social History
  (`icon-social`), Family History, Outside Care. In Social History, alcohol Yes reveals
  `.form-input.number.beverages`, smoking Yes reveals `.form-input.number.cigarettes`.
- Labs history: on a subsequent prenatal encounter, Laboratory shows only the History task
  while earlier labs are pending; each row's UPDATE opens `#prenatal-labs-history/...`.
- Prenatal HIV PCR is offered only at a visit after one where HIV was found positive (or known
  positive). Its question "Are there less than 20 copies/mm3?" is `.form-input.yes-no.hiv-level-undetectable`;
  No shows the input `.form-input.measurement.hiv-viral-load`. Partner HIV, HIV and HIV PCR tabs share
  one icon, so pick tabs by text. A sample sent to the Lab makes Next Steps end with Wait → Pause
  Encounter, back on the main menu.
- Labs history rows are `.history-entry` (name, date, and an `.action` reading UPDATE).
- Prenatal History → Obstetric is in two steps. Step 1 holds the counts
  (`.form-input.number.<term-pregnancy|preterm-pregnancy|stillbirths-at-term|stillbirths-pre-term|abortions|live-children>`).
  If every count is 0 and the patient is currently pregnant, the button reads Save and step 2
  (`.form.history.obstetric.second`) is skipped; otherwise it reads Save and Next. `completeHistory`
  only enters fixed counts, so other answers are filled by hand. History stays in To Do until
  Medical is saved, and stays editable until the encounter ends.
- A nurse's prenatal diagnoses are recomputed on every save once Danger Signs is done. The
  preeclampsia-risk diagnoses also need EGA ≥ 12 weeks; age under 18 or 35+ on a first pregnancy
  gives Moderate Risk on its own, so use age 25. These diagnoses raise no popup: read them on the
  progress report.
- Prenatal progress report: Reports tab → Clinical Progress Report (`h.progressReport.openReport(page,
  'prenatal')`). The Obstetric History pane is the first `.risk-factors`; diagnoses are under
  OBSTETRIC DIAGNOSIS. The encounter page's red `div.alerts` icon opens `.alerts-dialog` (High Risk
  Factors, such as convulsions at a previous delivery). The icon is absent when there is no alert.
- An encounter's `field_scheduled_date` holds the start in `value` and the end in `value2` (null =
  still open); the tablet's `scheduled_date` mirrors it. Editing either with drush and syncing
  moves the visit on the device. For a visit started yesterday and still open, set only `value`:
  `backdateEncounter` sets both, which ends the visit.
- Prenatal participant page buttons: nurse — FIRST ANTENATAL ENCOUNTER / SUBSEQUENT ANTENATAL
  ENCOUNTER / POSTPARTUM / RECORD PREGNANCY OUTCOME; a CHW's first reads FIRST ANTENATAL VISIT.
  `qa.state()` marks the off ones `[disabled]`. Postpartum is on for a woman with no visits, so
  no backdating is needed to reach one. A CHW viewing a pregnancy with only nurse visits
  gets the `.open-pregnancy-popup` warning; its "No - document pregnancy outcome" works only once
  a visit has ended.
- Pregnancy Outcome inside a postpartum visit closes the pregnancy on Save and returns to the
  still-open visit. The participant page then ignores that pregnancy (B-373).
- CHW visit 2 with no danger signs: Danger Signs, Birth Plan, Health Education, Next Steps; Next Steps
  is Appointment Confirmation, CHW Follow Up, Next ANC Visit. Next ANC Visit shows a date computed
  from the LMP; saving it as the last task opens the progress report, which carries End Encounter.
- CHW postpartum visit: Pregnancy Outcome, Danger Signs (tick "None of these" for mother and child),
  Next Steps. Postpartum Danger Signs has two "None of these" lists, so `completeDangerSigns` fails
  there. With no danger signs Next Steps is Health Education, plus Newborn Enrolment after a
  live birth — record a stillbirth (outcome option 3) to skip it. Saving Next Steps opens the
  progress report, which carries End Encounter. Ending either role's postpartum visit lands on the
  main menu.
- A second encounter on the same day is impossible: backdate the first with
  `h.common.backdateEncounter(name, type, days)`, then sync before starting the next.
- Well Child Head Circumference: its tab shows only for children under 36 months. Under the
  input, "Previous measurement: <cm>" or "No previous measurement on record". "Not taken" is
  stored as 0 cm with a `not-taken` note. A nurse's existing child is reopened through Clinical →
  Individual Encounter → Standard Pediatric Visit → search → forward icon, and a new visit
  starts with "Standard Pediatric Visit Encounter".
- On the device, IndexedDB `sync` → `shards` holds each measurement with `type`, `person` (uuid),
  `date_measured`, `uuid`, `vid` and its own fields under the server names (`test_execution_note`,
  `test_prerequisites`, `sugar_count`) — no `nid`: the way to confirm what the tablet itself has.
- Server fields of a lab test: `field_test_execution_note`, `field_test_prerequisites` (multi-value),
  `field_sugar_count`. A drush edit through `entity_metadata_wrapper(...)->save()` reaches the
  tablet on the next sync.
- **Immunisation forms** (Well Child Immunisations, Child Scorecard Vaccination History) have one
  tab per vaccine. The tab list hides a vaccine until its next dose is due, judged from the latest
  dose by number. "Received prior to today? Yes" opens Select Date: a picker whose year and month
  lists hold only the allowed range, from one interval after the latest dose to yesterday. Days
  outside it are `td.date-selector--disabled`. Each saved date returns to the form, which asks
  again while another dose is due. Answer No to stop. A CHW Well Child visit asks only about earlier
  doses; the nurse's also asks about today. OPV dose 1 within 14 days of birth makes OPV four doses,
  so record a first dose later than that for three.
- **A child followed by both Well Child and Child Scorecard**: the CHW registers the child under
  Child Scorecard, at Akanduga, and the nurse finds the child at Nyange. A nurse's Standard Pediatric
  Visit creates the Well Child participant. Both forms read one combined history of the two
  programs. Its order on develop depended on which program held which dose, so try both arrangements.
- Child Scorecard offers Vaccination History only after NCDA is saved
  (`h.childScoreboard.completeNCDA`), and only while the history shows the child behind.
- An activity with several tabs stays under To Do until every tab is saved.
- Well Child Nutrition Assessment: Height / MUAC / Nutrition / Weight tabs, each with its own
  Save; the Rwanda ranges are printed above the inputs (height 25–250 cm, MUAC 5–99 cm,
  weight 0.5–200 kg).
- ANC Examination: Vitals / Nutrition Assessment / Core Physical Exam / Obstetrical Exam /
  Breast Exam. Height, weight, BMI and MUAC share one task and one Save; fundal height is on
  Obstetrical Exam behind "Is fundal palpable? → Yes". Neither form prints its ranges.
- **Group sessions start empty on a fresh device** ("This Group has no mothers assigned to
  it") even though the backend has participants. Build one: Attendance → Add New Participant
  → register the mother → her person page → Add Child → "is the parent of" → Save. The
  child's activities are then under PARTICIPANTS in the header → the mother's card → the
  baby icon.

## Patient Record

- Opened from Participant Directory: search, then the row's `span.patient-record` icon (beside
  the forward arrow). An adult's record opens on the Acute Illness pane (`.pane.acute-illness`):
  one `.entry` per illness with a diagnosis — "<diagnosis> / ONGOING|RESOLVED / DD/MM/YYYY" — or
  "No matches found". An illness without a diagnosis is not listed.

## Dashboard (CHW)

- Main-menu card "Dashboards" opens CHW SNAPSHOT for the signed-in village. It shows a spinner until the device holds the health centre's statistics.
- After a server reinstall no statistics exist: run `ddev drush advancedqueue-process-queue hedley_stats_calculate_stats --timeout=300` before pairing, and check the `cache` row `sync_stats_general_<hc nid>` exists.
- Month selector: `.month-selector span.icon-back:not(.rotate-180)` goes back a month, `.label` reads "September 2026". Cards are `.dashboard-card.chw`, value in `.value`.
- Akanduga belongs to Nyange (nid 4).

## Case Management

- Nurse panes: All / Contact Tracing / ANC Labs / NCD Labs; the forward icon on an ANC Labs entry
  opens the recurrent encounter page, whose Lab Results card is `div.card div.image` holding
  `.icon-task-laboratory`. The lab technician sees ANC Labs only,
  one row per order (`<person> | ORDERED | ANC Lab Results | →`), opening
  `#prenatal-recurrent-activity/<uuid>/laboratory`: Partner HIV / HIV / Syphilis - RPR /
  Hepatitis B / Malaria / Blood Group / Urine Dipstick / Hemoglobin / Random Blood Sugar.
- Pane filter buttons are `div.ui.segment.filters button`; an entry's tap target is
  `div.icon-forward`.
- **NCD Labs** lists an encounter while today is before its results record's resolution date
  (`ncd_labs_results`: `field_performed_tests`, `field_date_concluded`). Sending a test to
  the lab sets it 35 days out. The entry opens the recurrent encounter; its Lab Results card is
  `.icon-task-laboratory`, one tab per test still owed. When nothing is left, the page reads
  "To Do (0)" — it draws no `.card` elements to count.
- **Each pane decides for itself whether an encounter clears its entry** — read its
  `generate<Type>FollowUpEntryData` and the `limitDate` its view passes
  (`Pages/GlobalCaseManagement/View.elm`) before choosing a pane for a scenario:

  | pane | `limitDate` | a same-day encounter clears the entry? |
  |---|---|---|
  | Child Nutrition (Home Visit) | `currentDate + 1` | **yes**, at once |
  | Acute Illness | `currentDate + 1` | no; the follow-up must belong to the illness's LAST encounter, with a diagnosis |
  | Immunization (Well Child) | `currentDate` | **no** — the only pane where "an encounter took place today" is reachable |
  | Prenatal | `currentDate + 1` | as Acute Illness |

  In practice the Immunization entry is visible only on the day its Well Child encounter
  happened, which is also the day a same-day guard matters.

- Encounters that produce an entry:
  - **Child Nutrition (CHW) → Home Visit pane.** MUAC 11 (red), Nutrition "None of these",
    Weight, Height; Next Steps appears once all four are saved. Saving Follow Up ("1 Day")
    alone creates the entry. The nutrition participant page also offers HOME VISIT
    ENCOUNTER directly, beside NUTRITION ENCOUNTER. Follow Up options: 1 day, 3 Days, 1 Week,
    2 Weeks, No follow up; the helper always picks 1 day.
  - **Antenatal (CHW) → Antenatal Care pane.** Every CHW antenatal visit except postpartum has
    Follow Up in Next Steps. A danger sign makes Danger Signs' Save open Next Steps with a
    "refer to health center" warning: Send to Health Center, CHW Follow Up, Next ANC Visit, and
    no Health Education.
  - **Well Child (CHW) → Immunization pane.** Danger Signs, Nutrition Assessment, then
    Immunizations answering **No** to each vaccine — `h.wellChild.completeImmunisation(page,
    { isChw: true })`; without `isChw` it answers Yes and gives the vaccines. Leave them
    unadministered: a child given everything gets a future date and **no entry**. "BEHIND" on the progress report means the
    entry will show.
  - **Acute Illness (CHW, adult) → Acute Illness pane.** Fever + Chills; temperature 38.5;
    Malaria RDT positive, not pregnant → Uncomplicated Malaria; Next Steps: Coartem Yes and
    Follow Up "1 Day".

## CHW Dashboard

- Main menu → DASHBOARDS opens "CHW SNAPSHOT" for the CHW's village. It shows a spinner until the
  server's statistics for the health centre exist. A fresh local server has none: run
  `ddev drush eval "hedley_stats_calculate_stats_for_health_center(4);"` (Nyange), then sync.
- The Case Management cards (Acute Illness, Home Visit, Antenatal Care) and Malaria/Covid/GI
  "Currently in Care" come from follow-ups on the tablet. They change as soon as a visit saves.
- Every other card, including the Antenatal Care page's With Danger Signs, comes from those
  server statistics. A new visit shows only after upload, the drush recalculation and a sync.
- Cards are `.dashboard-card.chw` (`.header`, `.value`). The month label is
  `.month-selector .label`; previous month is `span.icon-back:not(.rotate-180)`, next is `.rotate-180`.
- Sub-pages: Antenatal Care / Child Nutrition / Acute Illness / Group Education buttons; Acute
  Illness has Overview / COVID-19 / Malaria / Gastro in `.page-filters`.

## Statistical Queries (admin)

- Village scope: `/admin/reports/statistical-queries/demographics/<province>/<district>/<sector>/<cell>/<village>`.
  It reads each person's `field_reports_data` as the page loads, so a recomputed person shows on reload.
  Global, province, district and health-centre scopes use precomputed data.
- Sign in with `h.reports.drupalLogin(page)`. Pick the report with the first `select.select-input`
  (`prenatal-contacts` is ANC Contact), then `h.reports.setDateRange`. Read rows with
  `h.reports.readPrenatalContactsTable` and `findSimpleRow`.
- Recompute one person: `hedley_reports_calculate_aggregated_data_for_person(node_load($nid, NULL, TRUE))`
  in a script run by `ddev drush scr`. The script must sit under `server/www/` (container path
  `/var/www/html/server/www/…`); delete it afterwards.
- Prenatal data sits under `individual.antenatal`. Each encounter is `date|nurse|<diagnosis codes>|<indicator codes>`;
  the codes are in `hedley_reports_prenatal_*_mapping()` (for example `h` is intra-uterine death).

## The QA driver

- State lives in `client/qa-recordings/.driver/` (gitignored): `profile/` (the browser
  profile, so pairing survives a restart), `shots/`, `last-url`, `driver.log`.
- Viewport, device metrics and timezone match the e2e recordings: iPad Mini user agent,
  820×1024 at scale 1, mouse input, UTC. The app lays pages out 800 px wide
  (`<meta name="viewport" content="width=800">`), so at iPad Mini's own 768 px the right
  32 px fall outside the view.
- The browser is headless unless started with `--watch`. On a 1080 px screen a visible
  window is only ~960 px tall inside, and the screencast — the video — keeps only that part.
- Commands run one at a time. `qa.sh` waits up to 600 s for a reply (`QA_TIMEOUT`); a long
  wait inside a command (`syncAndWait` allows 300 s) needs the Bash call's own timeout raised.
- A driver started from a worktree loads that worktree's helpers, which is how to try a new
  helper before it merges. Start it with `E2E_DDEV_PROJECT=/var/www/html/ihangane`, or the
  drush-backed helpers (`qa.freshDevice`, backdating) run `ddev` inside the worktree and fail.
- **Running the app on another date.** Once per driver session, `page.addInitScript` a function
  that replaces `window.Date` with a subclass adding `localStorage.qaDateOffset` ms to `now()` and
  to `new Date()`. Set the offset, then `page.goto('/')` plus `page.reload()`, since a hash-only
  goto does not reload. Measurements and visits then carry that date, and the backend stores
  it. The service worker keeps the real date (see pitfalls); `page.clock` must not be used.
- Only a caller that can read `.driver/token` (owner-only, new each start) can send commands;
  requests from a browser page are refused.
- Measured: empty profile → paired, signed in and synced in ~10 s; register a woman, open a
  first ANC encounter and reach the Random Blood Sugar tab, while recording, in ~17 s.
