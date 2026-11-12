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
- The tablet takes its site from each general download: `ddev drush vset hedley_general_site_name burundi`, then
  `syncAndWait`, and localStorage `syncInfoGeneral.site` reads `burundi`. No ddev restart is needed. Set it back to
  `rwanda` before the run ends.
- Drupal admin: `admin` / `admin`.
- GitHub CLI 2.99 or later is required: it is the first with `--attach` for videos, and
  older ones fail on this repo's `gh issue view` / `gh pr view` (Projects classic).
  With 2.103, `gh issue view <n> --comments` and `gh pr view <n> --comments` printed nothing, no error:
  read them with `gh api repos/TIP-Global-Health/eheza-app/issues/<n>` (and `/comments`, `pulls/<n>/comments`).
- Hedley scripts run as `ddev drush scr profiles/hedley/modules/custom/<module>/scripts/<file>.php`.
  Most take `--dry_run` and `--nid` (only records after that nid), so a run can be limited
  to the records it created. In your own `drush scr` script, `drush_get_arguments()` holds two
  entries before the script's own arguments (read from index 2).
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

- Stock Management needs `stock_management_hc` (nurse) or `stock_management_village` (CHW).
  A local database can still carry `hedley_admin_feature_stock_management_enabled`, which nothing reads.

## Accounts and roles

| account | PIN | after sign-in |
|---|---|---|
| Nurse Maya | 1234 | picks a health centre: Muhondo or **Nyange** (QA uses Nyange) |
| CHW Jojo | 2345 | picks a village: Akanduga at Mbirima, or Busake at Busake |
| Lab technician | 3333 | picks a health centre; the menu has only Case Management and Device Status |

- Server side, nurse nodes are Maya 190, Jojo 191, PhePhe 192 (nurse), YumYum 193 (CHW), LabTech 194.
  Every one holds both centres (`field_health_centers` 4 Nyange, 5 Muhondo); the CHWs hold
  `field_villages` 140 Akanduga and 165 Busake; 163 Buhara and 188 Gahama are unassigned.
  An `entity_metadata_wrapper` save makes a new vid, which the device downloads.
- Villages by health centre: Akanduga 140 and Buhara 163 are at Nyange (4); Busake 165 and Gahama 188 at Muhondo (5).
  For two villages of one centre, add 163 to Jojo's `field_villages` (wrapper save, sync), then set back to 140, 165.
- On the device a nurse is a `nodes` row with `type: 'nurse'`, `label`, `vid`,
  `health_centers` and `villages` (uuids). The location picker lists `p.select-location ~ button.ui.primary.button`.
- Switching account keeps the device paired: `qa.signIn(pin, location)` goes to the main
  menu, signs out, enters the PIN and picks the location — about a second.
- Individual encounters offered — nurse: Acute Illness, Antenatal Care, Child Nutrition,
  Noncommunicable Diseases, Standard Pediatric Visit. CHW: Acute Illness, Antenatal Care,
  Child Nutrition, Well Child Visit, Child Scorecard, TB Management, HIV Management.
- Family Encounter (flag `family_nutrition`) is CHW-only: Clinical → `button.family-assessment` → Nutrition Encounter.
  Its search lists adults of the chosen village.
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
- Sync Settings sits behind Device Status's top-left "Sync Manager" link (`summary` "Sync
  Settings"). Idle time 5000 ms, saved there, goes to localStorage `syncSpeed` and makes general
  downloads land within seconds — the way to sync on the PIN and location screens, which have no sync icon.
- Sync timing: the app's clock (`currentTime`) moves every 10 s, so a download is sent again 30–40 s after it was sent; after a failed reply the loop ticks at the offline pace (10 s). A `UserPage` never reloads itself after a long sync; Device Status can.
- Device Status shows each health centre in a `.health-center` block, with Last Successful
  Contact, Remaining for Upload, Remaining for Download and Status in `.sync-status`.
- "Status: Success" alone does not prove the upload: check "Remaining for Upload: 0" too.
  Queued changes are the IndexedDB `nodeChanges` and `shardChanges` rows.
- Device Status has "Error log" and "Sync Manager" debug panels behind its top-left links.
- **Stopping the service worker** (Chrome does it to an idle worker): `cdp = await page.context().newCDPSession(page)`,
  listen to `ServiceWorker.workerVersionUpdated` (the `activated` version's `runningStatus`), `ServiceWorker.enable`,
  then `ServiceWorker.stopAllWorkers` and wait for `stopped`. The next `/sw/` request starts it again.
- The page has the app's Dexie instance as global `dbSync`: `shardChanges` rows carry an auto-increment `localId`,
  so "rows added by this save" is `where('localId').above(<last before>)`, which a sync that deletes rows cannot fool.
  Service-worker save failures land in `dbErrors` (`Dropped <method> <type> <uuid>: …`).
- IndexedDB `sync` stores: `nodes` (general data only, ~36 rows), `shards` (authority data:
  persons, relationships, measurements), `nodeChanges` / `shardChanges` (queued uploads),
  `authorityPhotoUploadChanges` (`{localId, photo, fileId}`, `fileId` null until the file
  uploads), `deferredPhotos`, `syncMetadata`, `statistics`.
- The upload is a main-thread XHR to `/api/sync`, so `page.on('request')` in the driver sees
  its body (`request.postData()`). That is the only way to tell a dropped key from a null
  one, since the backend treats both alike. Downloads go through the service worker.
- **Holding a sync download in flight** (to land a save during it): `page.context().route(/\/api\/sync/, …)`
  sees the download GETs, though `page.route` does not. Downloads are `GET /api/sync` (general) and
  `GET /api/sync/<hc uuid>` (authority). The same URL with `statistics=1` is the dashboard-stats
  phase, which is not a download: let it through. The request times out at 30 s, so hold under 25 s.
  localStorage `syncSpeed` `{"idle":3000,"cycle":50,"offline":30000}`, then a reload, runs a cycle
  every ~3 s, so a GET is held within a few seconds.
- A server edit of a measurement (`entity_metadata_wrapper` save) reached an open encounter page
  about 7 s later with that sync speed.
- **Relationships load slowly after a fresh pairing.** A person page can show "Family
  Members: … Not Found" through several reloads before it starts working. It is a loading
  delay, not `clinics-not-found-bug.md`.

## Controls

- Main-menu cards: the click handler is on `div.card > div.image`, not the card or label.
- Clinical: `button.individual-assessment` / `button.group-assessment`.
- Participant search: type in `input.search-input`; each result is `.item.participant-view`.
  The mode is the `.search-by` radio pair (Name / National ID); switching clears the box. The count is
  `.results-summary` ("There is 1 participant that matches your search."), the results `.participants-list .item`.
  Results land about a second after the last key (debounce plus the device query). National ID matches the start of the ID.
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
  done, then `active`. Acute Illness and Antenatal then ask in an in-app "END ENCOUNTER?" modal
  (CANCEL / CONTINUE); Well Child and Home Visit end without one. None of these are native
  dialogs. Ending an antenatal visit lands on the main menu.
- A disabled button inside `div.actions` cannot be hovered (`.actions` takes the pointer), so
  `qa.click` on it times out: hover with `{ force: true }` to show it on video, and read its class.
- Date of birth: open `div.date-input.field`, set YEAR (it rebuilds the MONTH list), then
  MONTH, click the day, then the popup's SAVE — the day click alone does not close it.
  `h.common.setDate` does this.
- Photos: the widget is a Dropzone (`#dropzone`), not a camera. Give it a file with
  `setInputFiles` on its hidden input, `input.dz-hidden-input`, which sits on `body`, not inside `#dropzone`; the thumbnail turning into a `/cache-upload/images/<id>`
  URL means it took.

## Registration

- The nurse's form has the address cascade (Province → District → Sector → Cell → Village)
  and Registering Health Center; the CHW's form has no address section.
- Fields depend on the date of birth: "Mode of delivery" appears for a child, "Level of
  Education" / "Marital Status" for an adult. Set the date first.
- Address values: Amajyaruguru 2755 → Gakenke 2756 → Coko 2803 → Mbirima 2809 → Akanduga
  2810. Nyange HC is nid 4, uuid `725a9a6f-ded6-5e94-b914-94a221d2ba93` (Muhondo nid 5, `b6b0a893-…`); a
  record's `shard` on the device holds that uuid.
- `registerAdult` names everyone `E2ETest <firstName>` (Drupal title "secondName firstName").
  A nurse's registration takes the first option at each address level: Amajyaruguru →
  Gakenke → Busengo → Birambo → Birambo.
- Demo data: 86 persons, all with a migration photo; their national IDs are 16 digits starting `119`; demo children are born in 2022, too old
  for head circumference or the newborn exam — register a newborn for those.
- **Editing a demo person needs Level of Education and Marital Status**, which the migration
  left empty; the form refuses to save without them.
- National ID: Rwanda's field is a number input and accepts 16 digits only. Burundi's is free text, so letters,
  `/` and `-` need the Burundi site, or a server edit (`field_national_id_number`, reaching `shards` `national_id_number` on
  the next sync). Once the field holds more than 13 characters, Save warns "National ID … is already assigned to patient
  …" (CANCEL / CONTINUE) when a stored ID starts with it.
- Burundi registration: Second Name comes first; the address is Province → Commune → Zone → Colline → Sub-Colline. A
  CHW's registration fails there ("cell/district/… is a required field"): the local villages are Rwandan. Use the nurse.
  `registerChild` and `createChildAndStartWellChildEncounter` select the Rwandan labels (District, Sector, …), so walk the Burundi
  labels with `h.common.selectByLabel(page, label, 1)`; the first option at each level saves. "Health Center:" still lists Nyange.
- An address edit propagates to children only if each child is loaded: open the child's own
  person page first, then go back and edit the parent.
- Saving a photo measurement also sets the **person's** photo, server-side
  (`HedleyRestful*Photos`, around line 104). Re-read a person's photo fid after any photo
  activity before asserting on it.
- **GPS** (flag `gps_coordinates`, on locally): create and edit forms show a "GPS Info:" section.
  With a fix it reads "GPS Location: <lat> , <lng>" plus a Save GPS Location checkbox
  (`fieldset.gps-info input[type=checkbox]`); without one, only the heading. The registration
  helpers never tick it.
- No screen shows a person's stored coordinates: read `latitude` / `longitude` on the person's
  `shards` row, or `field_latitude` / `field_longitude` on the server. The server ignores
  `save_gps_location`, so a downloaded person always has it false.
- A nurse's Add Child: the child form takes the parent's address. On the relationship page,
  Save stays disabled until "Add to Group..." has a value.
- Person page: "Family Members:" lists each relative as "<name> (Child)" / "(Parent)", then "Add Child" (adult) or
  "Add Parent or Caregiver" (child), opened by `div.add-participant-icon-wrapper`. It leads to
  `#relations/<uuid>/<origin>`, headed "ADD FAMILY MEMBER FOR <NAME>". That search shows only the other age group
  and leaves out people already related. Result names: `.participants-list .item .details .ui.header`.
- A child linked to a mother on the server: `h.familyNutrition.addChildViaDrush(motherFullName, …)`. It stores
  mode of delivery `svd`, which jams sync (pitfalls); set `svd-no-episiotomy` before the next sync.
- WhatsApp report (flag `report_to_whatsapp`): "Send via WhatsApp" on a progress report → consent
  Yes → "Is this the correct number?" (only when the profile phone can receive a report; No) → number → Continue →
  "update the patient profile?" Yes patches the person's phone → sections → confirmation → Send →
  "Success…" → CLOSE. The NCD report has the button as soon as a visit starts (Reports tab → Progress Report).
- The dialog is `.modal.send-via-whatsapp`; the number field `input.phone-number` beside a country `select`.
  Section checkboxes: click the `input` inside `.ui.checkbox`; it covers the label.
- Send queues the report in IndexedDB `whatsAppUploads` (`phone_number` as sent, with country code);
  after upload it is a server `whatsapp_record` node (`field_phone_number`, `field_person`).
- A person's phone is `field_phone_number`; set with drush (`entity_metadata_wrapper`), it reached the
  tablet on the next `syncAndWait`.

## Encounters: what the helpers do not tell you

- Prenatal Laboratory: Random Blood Sugar is the last tab; performed today Yes → Point of
  Care → before meal → the mg/dL input appears. NCD Laboratory tabs: Random Blood Sugar,
  Creatinine, Urine Dipstick, HIV, Liver Function, Lipid Panel, HBA1C. Its Random Blood Sugar
  asks performed (`test-performed`) → `immediate-result` (Point of Care / Lab) →
  `patient-fasted` → `test-performed-today`; Lab ends on "update test results via case
  management". NCD HIV: known positive No → performed Yes → performed today Yes → result select; Positive adds `hiv-program` (5 tasks).
  Creatinine: performed → performed today, always to the lab. Urine Dipstick: performed →
  performed today → Short Dip / Long Dip. No reveals five why-not reasons. `completeLaboratory` answers
  every tab; to send only some tests, drive their tabs by hand and leave the rest unanswered.
- NCD recurrent Lab Results: Random Blood Sugar `.form-input.measurement.sugar-count`; Creatinine
  `creatinine-result` and `bun-result` (1 and 15 bring no Next Steps); Urine Dipstick three selects
  (protein, pH, glucose). The last result returns to the recurrent encounter page ("To Do (0)",
  LEAVE ENCOUNTER → Case Management). RBS 250 offers Next Steps (Medication Distribution);
  `completeRecurrentNextSteps` saves it. Lab Results stays openable from Completed, where a
  result can be saved again unchanged.
- NCD Next Steps appears only after Danger Signs, Symptom Review, Examination and Medical
  History are saved, even when a lab result already diagnoses (the four helpers do it). Its
  Medication Distribution names the diagnosis: "This patient has Diabetes with glucose … of
  250 mg/dL". The encounter's diagnoses are `field_ncd_diagnoses` (`diabetes-recurrent`).
- Acute Illness laboratory (nurse, initial encounter): the COVID rapid test is offered only
  while there is fever with respiratory symptoms (or two or more general ones) **and the
  malaria RDT is not positive** (`covid19SuspectDiagnosed`). Turning malaria Positive takes
  the COVID tab away. "Currently pregnant?" follows a positive result, only for a woman of
  child-bearing age. Saving either test re-diagnoses, with an assessment dialog (CONTINUE).
- CHW adult acute illness, Headache + Cough / Nasal Congestion / Sore Throat, RR 32, 37.0 °C: the
  **Acute Findings** save (last Physical Exam tab) diagnoses, not Vitals. It shows "Assessment:
  Suspected Uncomplicated Pneumonia" over the encounter page; Prior Treatment is still To Do. The
  device row (`acute_illness_encounter`) holds it as `acute_illness_diagnosis: ri-uncomplicated`.
  The progress report's Assessment section and the Patient Record read this stored value; the
  encounter header computes it from the measurements.
- **Acute Illness, more encounters on one illness the same day**: no backdating is needed. On the participant page, Existing Acute Illness → the illness button opens the role's open encounter, or starts one numbered next for the day
  (a nurse while a CHW visit is still open; a CHW once their own visit has ended). An illness is listed only once an encounter carries a diagnosis other than Fever of Unknown Origin or TB Suspect.
- Adult nurse encounter, Cough + Nasal Congestion, RR 18, 37.0 °C, no acute findings: the Physical Exam save gives "Simple Cold and Cough" (`cough-and-cold`). Ending a CHW encounter lands on the main menu.
- Simple Cold and Cough, CHW adult (Cough + Nasal Congestion, RR 18, 37.0 °C): diagnosed at the Physical Exam save, no Next Steps, End straight away. A nurse's child of 5 or 6 months with the same answers: the Physical Exam save opens Next Steps behind the popup (lemon juice Medication Distribution, Follow Up), so `completePriorTreatment` cannot run until that is done.
- A nurse opening a CHW's illness: Existing Acute Illness → button "Subsequent Acute Illness Encounter / CHW Referral / <diagnosis>" starts a `nurse-encounter` (a first visit: Symptoms, Physical Exam, Prior Treatment, Laboratory), not a subsequent one.
- Diagnosis labels: `DiagnosisMalariaUncomplicatedAndPregnant` shows the same words as plain uncomplicated ("Uncomplicated Malaria" / "Malaria without Complications"); the stored value (`malaria-uncomplicated-pregnant`) is the only place that tells them apart.
- Acute Illness encounter page: header `.ui.basic.segment.head h1` ("Acute Illness Encounter" for a first visit, "Subsequent Acute Illness Encounter: <diagnosis>" for a follow-up), `.diagnosis-wrapper .diagnosis`, cards `.card .icon-task-<activity>`.
  An ended encounter's page is reached only through browser Back. The device row `acute_illness_encounter` holds `sequence_number`, `ai_encounter_type` (`chw-encounter` / `nurse-encounter`) and `acute_illness_diagnosis`.
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
- Labs history rows are `.history-entry` (name, date, and an `.action` reading UPDATE). Random Blood Sugar is listed
  as "Blood Sugar". A result entered there is reported to the current visit's `past_prenatal_diagnoses`, and its report
  reads "<diagnosis> - diagnosed on <date> as a result of entering lab results from past encounter".
- A Pregnancy Dating save sends two requests together: the LMP (POST, PATCH when corrected) and a PATCH of the
  pregnancy (`individual_participant`) setting its EDD. The visit header shows EDD and EGA; reopen the form from
  Completed to correct it, which sends both again. Backend EDD: `h.common.queryPregnancyEdd(name)`.
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
- Aspirin is given in Next Steps → Medication Distribution ("Administer: Aspirin (150 mg)"), once a
  preeclampsia risk is diagnosed. Next Steps shows only after every activity but Malaria Prevention,
  Photo and Ultrasound is saved. `completeHistory` overwrites obstetric counts, so a first pregnancy
  needs Obstetric (all 0) and Medical filled by hand. Medication's Calcium tab starts at 14 weeks.
  The full first-visit helper chain needs EGA ≥ 28 weeks: `completeMentalHealth` waits for a card shown only then.
  Below 28 weeks, leave `completeMentalHealth` out and the rest of the chain runs (19, 20 and 25 weeks tried).
- With labs sent to the lab, Next Steps ends on Wait → Pause Encounter, which ENDS the visit (`scheduled_date.value2` = that day)
  and lands on `#pincode` drawing the main menu. A later day's results go in through Case Management › ANC Labs.
- BP 165/100 at 19 w 5 d gives `chronic-hypertension-immediate` + `high-risk-of-preeclampsia-initial`; at 36 w 5 d
  `gestational-hypertension-immediate`. Next Steps then has Medication Distribution, which `completeNextSteps` fills.
- Severe preeclampsia fixture: `completeExamination(page, { vitals: { sys: '165', dia: '115' } })`, then reopen Examination from Completed →
  Core Physical Exam → "Tender to Palpation right upper" (ticking it unticks Normal) → Save. At 28 w and at 37 w that visit stores
  `gestational-hypertension-immediate` until a protein result arrives (before #2511 also `high-risk-of-preeclampsia-initial`).
- High risk of preeclampsia comes from chronic hypertension only (before 20 w, at the visit, the recheck or an earlier visit) since #2511;
  gestational hypertension alone gives none. "Hypertension before pregnancy" in Medical History, twins, diabetes and previous preeclampsia still do.
  Medication Distribution's hypertension header reads "Chronic hypertension" or "Pregnancy-Induced hypertension".
- Prenatal Urine Dipstick tab: performed Yes → `immediate-result` (first label Point of Care, or Lab) → Short Dip. Point of Care
  shows three selects: protein, pH, glucose; protein and glucose share the options 0, +1 … +4. Lab Results shows the same selects.
  At 37 w+ a point-of-care result that diagnoses an emergency jumps to Next Steps behind the warning, leaving the other lab tabs unasked.
- Eclampsia fixture: Dating, History, Examination, then Danger Signs "Convulsions" (no other activity needed). After 165/115 it also gives high risk
  of preeclampsia on develop; gestational hypertension is not stored beside eclampsia.
- Moderate risk of preeclampsia from BMI: reopen Examination from Completed → Nutrition Assessment → weight 80 (height 160 from the helper) → Save.
  BMI 31 gives moderate risk on its own, also beside gestational hypertension at 28 w (since #2511), with Aspirin. Beside chronic hypertension only high risk is stored.
- "Hypertension before pregnancy": after `completeHistory`, reopen History from Completed → Medical History tab → tick it (it unticks that group's
  None of the above) → Save. `.ui.checkbox` wrappers carry no `checked` class; read `qa.state().choices`.
- Two-visit fixture: earlier visit on a page clock 14 days back, upload, `backdatePrenatalEncounter(name, 14)` to end it, sync, reload, then
  Subsequent Antenatal today. A later visit after hypertension has Treatment Review (Prenatal Medication, Hypertension Medication: still taking,
  missed doses, adverse events; adverse Yes adds a hospital-referral question). Its Medication Distribution reads "previously diagnosed with
  Hypertension … remain unchanged" and lists Carvedilol/Amlodipine steps; the report adds "(continued)".
- With Healthy Start on, End Encounter stayed disabled after the full first-visit helper chain (Ultrasound, Malaria Prevention, Photo left in To Do).
- Same-day vitals recheck: visit 145/95, Next Steps ends on Wait › Pause; then Case Management › ANC Labs › the entry › Examination
  (`completeRecurrentExamination(page, { sys, dia })`). Recheck 145/95 stores `gestational-hypertension-recheck` at 20 w+, `chronic-hypertension-recheck`
  below; Next Steps › Medication Distribution offers Methyldopa. 165/100 schedules no recheck.
- Later lab result, one test only: `completeLaboratoryNurseForLab(page, { onlyTabs: ['Random Blood Sugar'] })`, press `.icon-back`, then
  `completeLaboratoryNurse` answers the other tabs No. A non-emergency Lab Results save returns to the recurrent encounter page, not Next Steps.
- Recurrent Next Steps: the Medication Distribution tab is found by its label, not `icon-next-steps-treatment`. After its last save the page
  returns to the recurrent encounter; the report is under its Reports tab.
- Medication record: server `prenatal_medication_distribution`, `field_prescribed_medication` (`aspirin`, `none-recurrent`) and
  `field_recommended_treatment` (`methyldopa-2`). The visit and its recheck share one record.
- Vitals recheck at 38 w: visit 145/95 with protein +1 at the visit stores only `depression-not-likely`; Next Steps is Health Education, Next ANC
  Visit, Wait. ANC Labs lists "Pending Vitals Recheck"; the recurrent Examination recheck 145/95 gives `moderate-preeclampsia-recurrent-ega-37+`,
  the immediate-delivery warning and Next Steps › Referral only.
- A hospital referral in the visit's Next Steps comes without a Wait task even while labs are pending: the encounter page then shows
  PAUSE ENCOUNTER enabled, which ends the visit and keeps the ANC Labs entry.
- EGA lines are not all "≥": diabetes vs gestational diabetes splits at `egaWeeks > 20`, so 20 w 1 d is still diabetes.
  Read the comparison in `Pages/Prenatal/Activity/Utils.elm` before choosing a fixture's weeks.
- With Healthy Start on, a nurse visit has an Ultrasound activity: viable, fetuses, location, the scan
  date, and weeks + days by scan. Saving it re-dates the pregnancy from the scan. The scan date is
  `field_execution_date` on `prenatal_ultrasound`.
- Healthy Start GWG: the Nutrition Assessment form of a later visit shows "GWG Classification:
  Inadequate" when the gain since the previous weight falls short. Move the earlier visit and its
  measurements first (pitfalls, backdating).
- The verdict is `.gwg-value` (Adequate green / Inadequate red); each Nutrition save stores it on the encounter's
  `prenatal_indicators` (`adequate-gwg` / `inadequate-gwg`, server `field_prenatal_indicators`). Expected gain is
  23.5 g/day before 13 weeks, 60 g/day after (73 if underweight), from the previous nurse visit's start to this visit's start.
- With Healthy Start off the same form shows the four-level verdict and "Pre-pregnancy baseline BMI". The flag reaches the
  tablet after `syncAndWait` and a reload; the Ultrasound card then goes.
- A nurse visit's save day can differ from its visit day without drush: shift the page clock (The QA driver), open the
  visit on one day and reopen it later. Subsequent Antenatal reopens an open nurse visit from any earlier day.
- Progress report panes: chronic hypertension and diabetes are under `.medical-diagnosis`; gestational hypertension
  ("Pregnancy-induced hypertension"), gestational diabetes and preeclampsia under `.obstetric-diagnosis`.
- An ended visit's progress report: Participant Directory → `span.patient-record` → `.pane.progress-reports` button
  Antenatal Care → `.entry.antenatal .icon-forward` (one row per pregnancy). It writes nothing.
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
  `qa.state()` marks the off ones `[disabled]`, but read straight after navigating it can show
  stale marks and list the hidden "YES / NO - DOCUMENT PREGNANCY OUTCOME" popup buttons; check the
  screenshot when it matters. Postpartum is on for a woman with no visits, so
  no backdating is needed to reach one. A CHW viewing a pregnancy with only nurse visits
  gets the `.open-pregnancy-popup` warning; its "No - document pregnancy outcome" works only once
  a visit has ended.
- CHW visits 2 and 3: the day after a visit 1 left open, Subsequent Antenatal is offered. Visit 2
  is Danger Signs, Birth Plan, Health Education, Next Steps; visit 3 has no Birth Plan. Visit 3's
  Health Education also asks `family-planning`, which `completeHealthEducation(page, 3)` leaves
  unanswered. Without danger signs, Next Steps is the card "Appointment Confirmation" with tasks
  Appointment Confirmation, CHW Follow Up and, once the pregnancy is dated, Next ANC Visit.
- Next ANC Visit shows a date computed from the EGA at the visit's start (`.ui.form.next-visit .date`;
  EGA 20–25 weeks gives six weeks on) and saves it to the encounter's `field_next_visit_date`.
  Saving the last Next Steps task opens the progress report.
- Nurse Danger Signs "Convulsions" diagnoses Eclampsia, an emergency referral: Next Steps opens at once
  behind a warning (CONTINUE), and End Encounter then needs only Next Steps, not the other activities.
  The device row `prenatal_encounter` holds `prenatal_diagnoses: ["eclampsia"]`; the progress report
  lists it under Obstetric Diagnosis. On a re-paired tablet, FIRST ANTENATAL ENCOUNTER reopens the open visit.
- CHW Child Nutrition, 2 years, height skipped, 8 kg, MUAC 11, Edema: the Nutrition save shows the
  "Severe acute malnutrition, severely underweight with danger signs" popup and opens Next Steps. The page
  stores an assessment itself; the follow-up re-save adds `danger-signs-present` (`nutrition_nutrition` row,
  `nutrition_assesment`). Without it the stored value reads `danger-signs-not-present`.
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
  Well Child measurements point at their visit through `field_well_child_encounter`; set each one's
  `field_date_measured` to the same day (`node_save`), and the device shows the new dates after a sync.
- Well Child Head Circumference: its tab shows only for children under 36 months. Under the
  input, "Previous measurement: <cm>" or "No previous measurement on record". "Not taken" is
  stored as 0 cm with a `not-taken` note. A nurse's existing child is reopened through Clinical →
  Individual Encounter → Standard Pediatric Visit → search → forward icon, and a new visit
  starts with "Standard Pediatric Visit Encounter". A CHW's Well Child visit also has the tab (at 12
  months, between Height and MUAC), so pass `headCircumference` to `completeNutritionAssessment`.
- Head circumference for a boy aged 24 months: 38 cm is z −7.53 (microcephaly), 55 cm z 4.96
  (macrocephaly), 48 cm normal. Beyond ±3 a `div.ui.active.modal.danger-signs-popup` warns, and the
  reading saves only on its CONTINUE. The warning is kept on the visit (`field_encounter_warnings`;
  on the device the `well_child_encounter` row's `encounter_warnings`), not on the measurement.
- Nurse ECD at 24 months: 31 yes/no questions. All No gives an encounter-page popup ("Child is behind
  on ECD milestones…", CONTINUE) and the warning `warning-ecd-milestone-refer-to-specialist`.
- Well Child progress report (`#reports-tab`): Active Diagnosis lists the visit warnings as
  `.entry.diagnosis` — "Microcephaly ONGOING 14/10/2026", "Missed ECD Milestone - 24 Mo …" — or
  "No matches found". REVIEW PRIOR DIAGNOSIS swaps in the Prior Diagnosis pane (`.pane.diagnosis button`); its back arrow
  (`.head .link-back`) returns to Active. A child's Patient Record draws the same report (`div.page-report.well-child`), on
  a full load too, so it is the way to read a child with no open visit.
- A nurse visit where only head circumference is saved can be left open; a later day's "Standard Pediatric Visit Encounter" starts
  a new visit. To put an earlier visit in the past, set the visit's `field_scheduled_date` value and value2 and each measurement's
  `field_date_measured` (`field_well_child_encounter`) by drush, after upload; `drush eval "eval(base64_decode('…'));"` keeps the php quoting simple.
- Well Child progress report growth charts (`.growth-charts svg`, title `text.gender`): under 26 months Length-for-age,
  Weight-for-age, Weight-for-length 0–2 and head circumference (0–13 weeks below 13 weeks); under 60 months the 0–5
  set; older, Height-for-age 5–19, Weight-for-age 5–10, HC 0–5. Read geometry from the SVG: labelled lines `line.st18`,
  inner `line.st19`, left labels `text.st16` at x 95.4252/88.4252 (y = transform y − 2), the child's dots are circles
  filled `#06B9FF`; the plot runs y 506.7 (minY) to 119.9 (maxY) in `ZScore/View.elm`'s configs.
- The first `svg` in `.growth-charts` is the 0×0 markers element (`viewMarkers`): waiting for it to be visible times out. Wait for
  `.growth-charts svg line.st18` attached, and count charts from index 1. Gridlines and labels come from the config alone, not the child's data.
  `text.st16` also holds the x-axis month numbers: take y-axis labels by x (95.4252, 88.4252 or 745.0155 on the right).
- Chart SVG for geometry checks: the child's dots are `circle` without a class (`circle.z-score-white` are label
  backgrounds), joined by `polyline.child-data`; reference curves are `polyline.*-line-new` in the order −3, −2, (−1), 0,
  (+1), +2, +3; x-axis `line.unit-line` (labels `text.st20`) and `line.unit-breakdown-line` (labels at y 516.5436);
  z-score labels are `text.st23`. The plot runs x 110.9 to 715.4; a point outside the age range is drawn on that edge.
- Age-chart fixtures: register with the exact birth date by hand (`h.common.setDate` on `.date-input`); the helpers take
  whole months only. WHO −2 SD values come from `client/src/assets/z-score/*anthro.txt` (M·(1+L·S·z)^(1/L)).
- Well Child Next Visit shows the earlier of the ECD date and the medication date. From 6 months Vitamin A puts the medication date six months on, so the ECD date shows only where it comes first (the 15-month rung).
- `h.wellChild.createChildAndStartWellChildEncounter` always starts the visit; `h.common.registerChild(page, 'Well Child Visit', 'well-child', …)` stops on the participant page. To stop on the
  participant page, register by its steps up to `div.page-participant.individual.well-child`.
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
- Immunisation form controls: tab `#tasks-bar .link-section:has(.icon-<bcg|opv|dtp|pcv13|rotarix|tetanus>-vaccine)`;
  form `.ui.form.vaccination`; history rows `.history-entry` (`.dose`, `.date`, `.delete` only on this visit's latest
  dose). The date field `.form-input.date` opens the calendar; its SAVE, then `.update.actions` Save, record the dose.
  "Will receive today? Yes" hides the "prior to today?" question. A tab with nothing left to ask reads "0/0 Tasks
  Completed" with Save on. Prenatal Tetanus dose 1's range starts at the mother's birth date: pick the year and month first.
- A child exactly 70 days old (born 10 weeks ago) gets DTP, PCV 13 and Rotarix dose 2 due today once dose 1 is dated
  on day 42; a CHW sees a Well Child Visit at that age (not the newborn exam). `completeNCDA` works for such a
  Child Scorecard child, which is then offered Vaccination History alone.
- **A child followed by both Well Child and Child Scorecard**: the CHW registers the child under
  Child Scorecard, at Akanduga, and the nurse finds the child at Nyange. A nurse's Standard Pediatric
  Visit creates the Well Child participant. Both forms read one combined history of the two
  programs. Its order on develop depended on which program held which dose, so try both arrangements.
- **Duplicate participants** (two `individual_participant` nodes, one person, one type) are built on the
  server: a node with `field_person`, `field_encounter_type`, `field_expected` (start date), `field_uuid`
  and `field_deleted` 0; shards fill in on save. An encounter moves by `field_individual_participant`.
- Develop's participant pages pick the participant with the LOWEST uuid (Dexie order), so a chosen uuid
  (`00000000-…` or `ffffffff-…`) decides develop's pick. Since #2455 new encounters go on the earliest start date.
- An encounter or participant with `field_deleted` 1 drops out of the tablet's queries after a sync: the way
  to hide a stray record from a fixture, and to bring it back.
- From the main menu, an in-app jump to `#well-child-encounter/<uuid>` (location.hash) showed a spinner for a
  minute. Reach the visit through its participant page.
- Child Scorecard offers Vaccination History only after NCDA is saved
  (`h.childScoreboard.completeNCDA`), and only while the history shows the child behind.
- **NCDA Antenatal Care step with recorded visits** needs a child linked to a pregnancy (`field_newborn`) that has prenatal
  encounters. CHW at Akanduga: register the mother, First Antenatal Visit with Pregnancy Dating only, leave; sync, set the
  encounter's `field_scheduled_date` value and value2 to a past day (ends it), sync, reload; Subsequent Antenatal is offered
  again, twice; then Postpartum → `completeChwPostpartumVisit` enrols the newborn. About 1 minute in all.
- The step shows the visits' start dates and, on Yes to "not recorded above", a 9-month grid (`.form-input.anc-months .item`:
  `.month-number`, `input` classes `checked`/`disabled`, tap target `.month-radio label`). Month = `Date.diff Months` from LMP
  (EDD − 280 days, or birth − 280 days without a pregnancy) + 1; a picked month stores LMP + N−1 months + 14 days. The counseling
  message is `.label.counseling`, shown while recorded visits + picked months < 4.
- The step is asked only while the child has no NCDA from any host (Child Scorecard, Nutrition, Well Child, group). Marking
  the saved NCDA node `field_deleted` 1 and syncing asks it again, so one fixture child serves several hosts.
- Nutrition NCDA (nurse at the health centre) opens behind a "proceed?" modal and has no Nutrition Assessment step.
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
- Group activity page (activities icon in the header → a card's `.icon-task-<name>`; Child Scorecard is
  `icon-task-history`, nurse only, and first asks "YES, PROCEED / NO, SKIP" on the activities page). Children are
  `.participant.card` (the shown one `.active`), in uuid order, not registration order; tabs are `.ui.tabular.menu .item`.
  The activities page keeps its tab, and an activity everyone finished moves to its Completed tab.
- Weight's form prints "Previous measurement: <kg>" from the child's individual history: the visible sign the child's
  data is loaded. On develop, a tap, filter or tab switch loads one level only, so it shows only for the default child.
- A group Nutrition save that assesses opens Next Steps behind a `.diagnosis-popup` (CONTINUE). From the Activities page that page has no back link:
  each task saves with `div.actions.next-steps button`, and the last returns to the activity page. On a child's own page the same tasks are
  tabs on that page (Contributing Factors, Health Education, Send to Health Center, Follow Up), each saving with `#save-form`.
  The assessment is kept on the `follow_up` node; the `nutrition` node's own `field_nutrition_assesment` stays empty when the Nutrition
  save completes the set for a child whose history was already loaded.
- A mother's FBF Mother tab shows only after Lactation is answered Yes. Tabs reachable: Family Planning, Lactation, then FBF Mother.
- A child's group Progress Report: the child's page → `Progress Report` tab → `a:has(.icon-progress-report)`. Attendance lists mothers only after "Display all mothers / caregivers" or a filter.
- `ul.links-head` (attendance, participants, activities icons) is on the Attendance, Participants and Activities pages only, not on a child's or mother's page: press `.link-back` first.
- Attendance row: `.item` with the mother's name; `.icon-checked-in` (checked in, tap to check out) or `.link-check-in` (not checked in). The mother leaves Participants when checked out.
- Hold or fail a group save without the server: wrap `XMLHttpRequest.prototype.send` in the page for POST/PATCH `/sw/nodes/<type>` (those go through the service worker, so `page.route` misses them); `client/e2e/group-session-save-button.spec.ts` has `installSaveControls`.
- Registering a mother and child for a group session (`createMotherOnAttendancePage` + `addChildToMother`) takes about 15 s and reloads the page.
- The child-data load for one child requests `/sw/nodes/pregnancy-by-newborn/<child uuid>` (`context.on('request')` sees it).
- CHW TB visit with a follow-up: `startTBEncounter`, `completeDiagnostics` `positive-pulmonary`, `completeMedication`, `completeNextSteps`, `closeReport` if open, `endTBEncounter`.
  About 40 s, ending on the main menu. HIV is the same chain with `positive-reported`. Both start from the participant page.

## Patient Record

- Opened from Participant Directory: search, then the row's `span.patient-record` icon (beside
  the forward arrow). An adult's record opens on the Acute Illness pane (`.pane.acute-illness`):
  one `.entry` per illness with a diagnosis — "<diagnosis> / ONGOING|RESOLVED / DD/MM/YYYY" — or
  "No matches found". An illness without a diagnosis is not listed. An entry's forward icon opens the progress report of the illness's
  latest encounter, never an earlier one.

## Case Management

- Nurse panes: All / Contact Tracing / ANC Labs / NCD Labs; the forward icon on an ANC Labs entry
  opens the recurrent encounter page, whose Lab Results card is `div.card div.image` holding
  `.icon-task-laboratory`. The lab technician sees ANC Labs only,
  one row per order (`<person> | ORDERED | ANC Lab Results | →`), opening
  `#prenatal-recurrent-activity/<uuid>/laboratory`: Partner HIV / HIV / Syphilis - RPR /
  Hepatitis B / Malaria / Blood Group / Urine Dipstick / Hemoglobin / Random Blood Sugar.
- Pane filter buttons are `div.ui.segment.filters button`; an entry's tap target is
  `div.icon-forward`.
- **Nurse after the lab technician**: once every ordered test is answered, the nurse's ANC Labs entry opens a
  "LAB RESULTS" page (`div.page-report.clinical`: patient info, a results table, REVIEW & ACCEPT), not the diagnoses
  (`openLabsResultsReviewFromCaseManagement`). "Review & Accept" opens the recurrent encounter; its Reports tab has the diagnoses.
  The entry stays listed as reviewed and then opens the encounter directly; Lab Results sits under Completed.
  With only the Urine Dipstick ordered and an emergency diagnosis (severe preeclampsia at 37 w), the nurse's entry opened Lab
  Results' Next Steps directly, behind the warning, with no LAB RESULTS review page.
- Lab technician and nurse share one tablet: `qa.signIn('3333', 'Nyange')`, then `qa.signIn('1234', 'Nyange')`.
- The lab technician's "Will this test be performed today?" No offers Lack of Reagents, Lack of Other Supplies,
  Broken Equipment, No Equipment, Not Indicated, and for Partner HIV also Not Present. Any lab technician save on
  HIV, Syphilis or Partner HIV, a decline included, lists that test in `field_tests_with_follow_up`.
- Read-only labels (`viewLabel`) end in a colon: "Will this test be performed today:", "Why not:". The question
  above a yes/no input ends in "?".
- **ANC Labs** (nurse) lists an encounter while today is before its `prenatal_labs_results` resolution
  date, labelled "ANC Lab Results", or "Vitals Recheck" while the recheck is owed. First-visit
  Vitals with systolic 140–159 or diastolic 90–109 schedules the recheck; 145/95 does it.
- On the device, `shards` rows `prenatal_labs_results` / `ncd_labs_results` carry `performed_tests`,
  `completed_tests` and `date_concluded`; the server fields are `field_performed_tests` and
  `field_date_concluded`.
- Record Acute Illness Outcome: Existing Acute Illness → Record Acute Illness Outcome → the illness's button (its diagnosis) → outcome `select` → Save.
- Withdrawing a test: reopen Laboratory, its tab, "performed today?" No, tick a `.why-not` reason,
  Save. `completeLaboratoryNurseForLab(page, { onlyTabs: ['HIV', 'Syphilis - RPR'] })` orders
  only those tabs, by their label text, and leaves the rest unanswered on the activity. To finish the visit,
  answer each other tab "performed today?" No with the first reason; Malaria's No adds one more yes/no.
- At the recurrent phase an HIV result brings Next Steps (HIV health education), so the phase ends on that save.
  Negative Syphilis or Hepatitis B, or Hemoglobin 12, bring none: the last result's save ends the phase and opens
  the progress report, which carries End Encounter.
- Recurrent Examination (the vitals recheck) is offered whenever the first visit's BP was marginal, on any later
  day, and the phase cannot end until it is taken. On a later day the last lab result alone still closes the entry.
- After entering results, the lab technician lands on the progress report (SUBMIT RESULTS / EDIT RESULTS); Submit
  returns to Case Management. The nurse's Review & Accept saves the tablet's whole copy, changing only the review state.
- Every save of a labs entry sets its `date_measured` to the save day.
- Each save of a labs entry is a server revision: read `field_revision_field_completed_tests`,
  `field_revision_field_review_state` and `field_revision_field_date_concluded` by `revision_id` from `node_revision`.
- **NCD Labs** lists an encounter while today is before its results record's resolution date
  (`ncd_labs_results`: `field_performed_tests`, `field_date_concluded`). Sending a test to
  the lab sets it 35 days out. The entry opens the recurrent encounter; its Lab Results card is
  `.icon-task-laboratory`, one tab per test still owed. When nothing is left, the page reads
  "To Do (0)" — it draws no `.card` elements to count.
- Listing rows are `.follow-up-entry .name` under the NCD Labs filter. An entry edited on the server
  (performed, completed, `field_date_concluded`) reaches the tablet's `shards` row within seconds
  at syncSpeed idle 5 s; a concluded date 35 days out lists a closed entry again.
- **CHW panes list residents of the chosen village only**, by its roster (people whose five address fields match). Switching village
  with `qa.signIn` keeps the in-memory data, so the previous village's people stay loaded: the way to have a non-resident "loaded on the device".
- CHW Tuberculosis and HIV panes: a TB or HIV encounter's Next Steps Follow Up makes the entry ("Tuberculosis Follow up" / "HIV Follow up").
  A positive `ncd_hiv_test` or `prenatal_hiv_test` makes an HIV entry too, unless the person has an HIV follow-up (the service worker adds it, `src/js/nodes.js`).
- Case Management's loads list uuids in the path (`/sw/nodes/tuberculosis_encounter/<uuid>,<uuid>`); `context.on('request')` shows whose data a page asks for.
- **Each pane decides for itself whether an encounter clears its entry** — read its
  `generate<Type>FollowUpEntryData` and the `limitDate` its view passes
  (`Pages/GlobalCaseManagement/View.elm`) before choosing a pane for a scenario:

  | pane | `limitDate` | a same-day encounter clears the entry? |
  |---|---|---|
  | Child Nutrition (Home Visit) | `currentDate + 1` | **yes**, at once |
  | Acute Illness | `currentDate + 1` | no; the follow-up must belong to the illness's LAST encounter, with a diagnosis |
  | Immunization (Well Child) | `currentDate + 1` | **no** — only a Well Child encounter started after the entry's date clears it, so the visit that scheduled it does not |
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
  - **Well Child (CHW) → Immunization pane.** The child must be under two (`expectImmunizationFollowUp`), so a 2-year-old never lists. Danger Signs, Nutrition Assessment, then
    Immunizations answering **No** to each vaccine — `h.wellChild.completeImmunisation(page,
    { isChw: true })`; without `isChw` it answers Yes and gives the vaccines. Leave them
    unadministered: a child given everything gets a future date and **no entry**. "BEHIND" on the progress report means the
    entry will show. The entry is the `well_child_next_visit` that Next Steps saves
    (`h.wellChild.completeNextSteps`); without Next Steps there is none.
  - **A record that reaches the server late** (a CHW device, one fixture): upload a follow-up today, set `qaDateOffset` to −5 days, open the participant page (Home Visit, Well Child Visit, Subsequent Antenatal, Existing Acute Illness → Subsequent, TB/HIV Encounter all offered), start the encounter, set the offset back to 0, sync. The server gets a higher nid with an earlier date, as from an offline device.
  - Tapping YES on an entry sets that follow-up's `field_date_concluded`, which takes the entry off
    the pane. Clearing it on the server and syncing lists the entry again.
  - **Acute Illness (CHW, adult) → Acute Illness pane.** Fever + Chills; temperature 38.5;
    Malaria RDT positive, not pregnant → Uncomplicated Malaria; Next Steps: Coartem Yes and
    Follow Up "1 Day".

## Dashboards

- Main menu → DASHBOARDS (click handler on the card's `div.image`) opens "CHW SNAPSHOT" for the CHW's
  village. It shows a spinner until the server's statistics for the health centre exist. A fresh
  local server has none: run `ddev drush eval "hedley_stats_calculate_stats_for_health_center(4);"`
  (Nyange, ~1 s, synchronous; the `cache` row `sync_stats_general_4` gets a new `created`), then sync.
  Akanduga belongs to Nyange (nid 4).
- **Nurse dashboard**: the same card opens a menu of `.page-filters` buttons — Antenatal Care, Child
  Nutrition, Acute Illness, NCDs, Child Wellness, Group Education. Child Wellness → Nutrition
  ("PEDIATRICS") holds "# of Micro/Macrocephaly" and "Total Encounters Completed", all from server
  statistics, open visits included. A change shows only after upload, the recalculation and a sync.
- The Case Management cards (Acute Illness, Home Visit, Antenatal Care) and Malaria/Covid/GI
  "Currently in Care" come from follow-ups on the tablet. They change as soon as a visit saves.
- Every other card, including the Antenatal Care page's With Danger Signs, comes from those
  server statistics. A new visit shows only after upload, the drush recalculation and a sync.
- Cards are `.dashboard-card.chw` (`.header`, `.value`). The month label is
  `.month-selector .label`; previous month is `span.icon-back:not(.rotate-180)`, next is `.rotate-180`.
- Sub-pages: Antenatal Care / Child Nutrition / Acute Illness / Group Education buttons; Acute
  Illness has Overview / COVID-19 / Malaria / Gastro in `.page-filters`.

## Stock Management

- Main menu → STOCK MANAGEMENT (`h.stockManagement.navigateToStockManagement`). A fresh local
  database has no `stock_update` nodes, so the page shows empty months until stock is received.
- History lists the 12 months before the current one (newest first); each row's VIEW opens that
  month's details. Both pages have `.month-selector`; a hidden arrow is `visibility: hidden`.
- `completeReceiveStock` always dates the receipt today. Receive Stock's date picker reaches five
  years back, so a past month's receipt can be entered through the form with `h.common.setDate`.
- Details rows: `.pane.month-details .row:not(.header)`; the signature cell holds only an image,
  so a row's joined `textContent` ends in an empty cell.

## Statistical Queries (admin)

- Village scope: `/admin/reports/statistical-queries/demographics/<province>/<district>/<sector>/<cell>/<village>`.
  It reads each person's `field_reports_data` as the page loads, so a recomputed person shows on reload.
  Global, province, district and health-centre scopes use precomputed data.
- Reached through the menu `/admin/reports/statistical-queries`: Scope select `all` / `demographics` / `hc`, then
  cascading selects (by label: Amajyaruguru → Gakenke → Coko → Mbirima → Akanduga; or Nyange Health Center), then
  the Load Data button in `.actions`. Results carry New Scope and a Report Type select.
- **Nutrition report**: eight tables, last 12 complete months (quarters, years for incidence), no date range. Large
  scopes read `report_data` nodes (`field_reports_data` JSON `nutrition_report_data`), one per province, district,
  health centre and Global. Build them: `generate-data-for-all.php` (person data, ~14 s locally), then
  `recalculate-large-datasets.php` (~15 s), both under `hedley_reports/scripts/`. A fresh local DB has neither.
- A person's nutrition encounter in `field_reports_data` reads `date stunting_z,underweight_z,wasting_z,muac,sign`
  (`individual.nutrition`); well-child ones are `date|…|`. Expected report counts can be worked out from these.
- Other reports need a start and end date before they render (`h.reports.setDateRange`). Download CSV fires a
  Playwright `download` event (`page.waitForEvent('download')`).
- Admin pages lay out 960 px+ wide: at the driver's 820 px the right columns of the 12-month tables are cut off.
  `page.setViewportSize({ width: 1280, height: 1000 })` shows them, in the video too.
- A server-only PR (module, compiled `hedley_general/js/elm-main.js`, `elm.css`) is live once the files are
  overlaid in the main tree and `ddev drush cc all` runs; gulp is not involved. Leave `server/elm/src` alone.
- Sign in with `h.reports.drupalLogin(page)`. Pick the report with the first `select.select-input`
  (`prenatal-contacts` is ANC Contact), then `h.reports.setDateRange`. Read rows with
  `h.reports.readPrenatalContactsTable` and `findSimpleRow`.
- Recompute one person: `hedley_reports_calculate_aggregated_data_for_person(node_load($nid, NULL, TRUE))`
  in a script run by `ddev drush scr`. The script must sit under `server/www/` (container path
  `/var/www/html/server/www/…`); delete it afterwards.
- Prenatal data sits under `individual.antenatal`. Each encounter is `date|nurse|<diagnosis codes>|<indicator codes>`;
  the codes are in `hedley_reports_prenatal_*_mapping()` (for example `h` is intra-uterine death).

## HealthyStart dashboards (admin)

- `/admin/reports/dashboards/facility` and `/program`, behind `hedley_admin_feature_healthy_start_enabled`.
  The first cohort date is `hedley_admin_healthy_start_first_cohort_date` (YYYY-MM-DD). Only pregnancies
  created on or after it get a record, so setting it to the run's day leaves older QA data out.
- Each pregnancy (`individual_participant`) holds its record in `field_reports_data`. Saves queue
  `hedley_reports_calculate_dashboard_data`, one item per pregnancy. Drain it with
  `ddev drush advancedqueue-process-queue hedley_reports_calculate_dashboard_data --timeout=30`;
  it finishes at once but then idles until the timeout.
- Sign in without a password: `ddev drush uli --uri=https://ihangane.ddev.site:4443` gives a one-time
  link, for Chrome or the QA driver's own page. A role-only user lands on its dashboard.
- Selects: `#filter-facility-site|time`, `#filter-program-location|site|time`, `select[id^=trend-kpi]`.
  Blocks are `#kpi-block-<id>`; a click opens `#dashboard-drill-down`. Facility opens on the first site by name (Muhondo).
- At the driver's 820 px the admin theme lays pages out 960 px wide, Statistical Queries too.
- Moving a person to another centre on the server: set the person's `field_shards`, then
  `hedley_patient_recalculate_shards_for_person_content($nid)`; the pregnancy and visits follow.

## The QA driver

- State lives in `client/qa-recordings/.driver/` (gitignored): `profile/` (the browser
  profile, so pairing survives a restart), `shots/`, `last-url`, `driver.log`.
- Viewport, device metrics and timezone match the e2e recordings: iPad Mini user agent,
  820×1024 at scale 1, mouse input, UTC. The app lays pages out 800 px wide
  (`<meta name="viewport" content="width=800">`), so at iPad Mini's own 768 px the right
  32 px fall outside the view.
- The browser is headless unless started with `--watch`. On a 1080 px screen a visible
  window is only ~960 px tall inside, and the screencast — the video — keeps only that part.
- Location: `page.context().grantPermissions(['geolocation'], { origin: 'http://localhost:3000' })`
  plus `setGeolocation({ latitude, longitude })` gives a fix; `clearPermissions()` makes the request
  fail as denied. Chromium caches fixes (see pitfalls).
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
  A shift of more than a year into the past works. Queued saves still upload once the offset is back to 0.
- A PR whose branch is behind develop, or stacked on another PR: build its tree with `git merge-tree --write-tree origin/develop origin/<branch>`
  and `git checkout <tree> -- <files>`; the control tree merges develop with the parent branch.
- The main tree starts a run on `develop`, so a develop control is cheapest before the overlay:
  record it on the served build, then overlay and wait for gulp once. For a change only to what a page shows, one fixture can
  serve both builds: overlay, build and read on the PR, restore the files (needed anyway), wait for gulp, activate the update in the same driver, read again.
- After an overlay, gulp copies `app.js` at once, but `Main.js` takes about 2.5 minutes to rebuild (a three-file Elm change once took
  seconds). Wait until `grep -F` finds a name from the overlay in `client/serve/Main.js`.
- The service worker precaches the bundle as `Main.js?__WB_REVISION__=<hash>`, so `caches.match('/Main.js')` finds nothing. To read the
  browser's cached build, walk `caches.keys()` and each cache's `keys()` for a URL matching `Main.js`.
- Only a caller that can read `.driver/token` (owner-only, new each start) can send commands;
  requests from a browser page are refused.
- `qa.freshDevice()` can fail once with "Navigation … interrupted by another navigation" on a fresh
  profile; running it again pairs normally.
- Measured: empty profile → paired, signed in and synced in ~10 s; register a woman, open a
  first ANC encounter and reach the Random Blood Sugar tab, while recording, in ~17 s.
