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
  opens the service-worker page, where a new build is applied. Each browser profile keeps
  its own service-worker cache, so two browsers can show two builds at the same time —
  always read the version in the browser doing the run.
- `EHEZA_SITE` (rwanda / burundi) is set in `.ddev/config.local.yaml`.
- Drupal admin: `admin` / `admin`.
- Hedley scripts run as `ddev drush scr profiles/hedley/modules/custom/<module>/scripts/<file>.php`.
  Most take `--dry_run` and `--nid` (only records after that nid), so a run can be limited
  to the records it created.
- A measurement node is found by its patient: join `field_data_field_person` to the person
  node and match its title (`E2ETest <firstName>`).
- A **server reinstall** (`server/install`) wipes every device, person and pairing, restores
  the migration devices (Tablet 1 `12345678`, Tablet 2 `87654321`) and turns the feature
  flags back on. A browser profile from before it holds credentials for a database that no
  longer exists: start the driver with `--fresh`.
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
- Search results and Participant Directory rows open through their `span.action-icon.forward`;
  the row itself is not clickable. The edit pencil (`span.action-icon.edit`) is on the person
  page, as is "Add Child" (`div.add-participant-icon-wrapper`).
- Save buttons: `button.ui.fluid.primary.button` with class `disabled` or `active`; the
  `disabled` attribute is never set, so read the class.
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
  Care → before meal → the mg/dL input appears. NCD Laboratory opens on Random Blood Sugar
  and first asks "Did you perform this test today?".
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
- A second encounter on the same day is impossible: backdate the first with
  `h.common.backdateEncounter(name, type, days)`, then sync before starting the next.
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

## Case Management

- Nurse panes: All / Contact Tracing / ANC Labs / NCD Labs; the forward icon on an ANC or NCD
  Labs entry opens the recurrent Lab Results page. The lab technician sees ANC Labs only,
  one row per order (`<person> | ORDERED | ANC Lab Results | →`), opening
  `#prenatal-recurrent-activity/<uuid>/laboratory`: Partner HIV / HIV / Syphilis - RPR /
  Hepatitis B / Malaria / Blood Group / Urine Dipstick / Hemoglobin / Random Blood Sugar.
- Pane filter buttons are `div.ui.segment.filters button`; an entry's tap target is
  `div.icon-forward`.
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
    ENCOUNTER directly, beside NUTRITION ENCOUNTER.
  - **Well Child (CHW) → Immunization pane.** Danger Signs, Nutrition Assessment, then
    Immunizations answering **No** to each vaccine — `h.wellChild.completeImmunisation(page,
    { isChw: true })`; without `isChw` it answers Yes and gives the vaccines. Leave them
    unadministered: a child given everything gets a future date and **no entry**. "BEHIND" on the progress report means the
    entry will show.
  - **Acute Illness (CHW, adult) → Acute Illness pane.** Fever + Chills; temperature 38.5;
    Malaria RDT positive, not pregnant → Uncomplicated Malaria; Next Steps: Coartem Yes and
    Follow Up "1 Day".

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
- Only a caller that can read `.driver/token` (owner-only, new each start) can send commands;
  requests from a browser page are refused.
- Measured: empty profile → paired, signed in and synced in ~10 s; register a woman, open a
  first ANC encounter and reach the Random Blood Sugar tab, while recording, in ~17 s.
