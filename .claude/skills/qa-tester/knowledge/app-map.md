# QA App Map — E-Heza

Stable facts about navigating and preparing the app for manual QA. Correct this file whenever
reality disagrees with it; add facts you had to discover the hard way.

For test accounts, page/activity selectors, and per-encounter-type mechanics, use
`../../e2e-test/references/e2e-knowledge-base.md` — that file is the source of truth for those.

## Environment

- App URL: `http://localhost:3000` (served by `ddev gulp` from the **main tree**
  `/var/www/html/ihangane` — it serves whatever branch that tree is on).
- After every Elm recompile: click **"Version"** in the app's top-right corner to activate the
  new code (service-worker update). Skipping this means testing the previous build.
- `EHEZA_SITE` env var selects rwanda/burundi (set in `.ddev/config.local.yaml`).
- Backend admin: Drupal at the ddev URL, `admin`/`admin`.
- A **server reinstall wipes every device, person and pairing** and puts the migration
  devices back: Tablet 1 code `12345678`, Tablet 2 code `87654321` (both single-use again).
  It also leaves the browser holding credentials for a database that no longer exists — wipe
  the app's local state before the next run or the upload lane jams (see pitfalls).
  Confirmed after the reinstall on 2026-08-21: two devices, no QA persons, feature flags
  back on. Mint a fresh device rather than spending a migration code — see SKILL.md Step 2.
  Re-pairing an already-paired device needs super user mode:
  `ddev drush vset hedley_super_user_mode 1` (set back to 0 after).
- Default nurse PIN: `1234`. Other accounts: see the e2e knowledge base accounts table.

## Feature flags

`ddev drush vset hedley_admin_feature_<name>_enabled 1|0` — flags: `ncda`, `stock_management`,
`tuberculosis_management`, `group_education`, `report_to_whatsapp`, `hiv_management`,
`gps_coordinates`, `family_nutrition`. A screen behind a disabled flag simply does not appear —
check the flag before concluding a screen is unreachable.

## Sync

Offline-first: the client writes to browser storage and syncs in the background. Backend
effects (nodes created, reports updated) are visible only after the upload lane drains. Verify
backend state with `ddev drush sqlq ...` / `ddev drush` queries.

## Navigation facts

(Append route recipes here as runs discover them: screen → click path from login.)

- **Every run pairs a fresh device** — see the drush recipe in SKILL.md Step 2. Codes are
  single-use, so pick a new one each time (77777777 and 88888888 are spent). A pairing does
  not survive reliably between runs: on 2026-08-21 the app came up on the pairing screen on
  its own after a rebuild, with the device node still intact in the backend, so never plan a
  run around a pairing that is already there.
- e2e person titles are `E2ETest <firstName>` (registerAdult hardcodes the second name and
  Drupal stores "secondName firstName").

## Sync (facts discovered 2026-08-20)

- Sync network requests go through the service worker and do not show in the tab's request
  log; judge sync by Device Status timestamps and the local `sync` IndexedDB instead.
- Sync Manager debug panels live behind the top-left "Error log" / "Sync Manager" links on
  the Device Status page; Sync Settings exposes Idle time (default 600000 ms — after an
  error the lane sleeps 10 minutes; lower it to retry quickly).
- After a wiped device pairs, general data (nurses) must download before any PIN works —
  "Your PIN code was not recognized" right after pairing just means sync hasn't finished.

## Navigation facts

- Register + first encounter (nurse): Clinical → Individual Encounter → <type> → search →
  Register a new participant → form (names, DOB via calendar popup, gender, education,
  marital status, NCD also Mode of delivery, address cascade Province→…→Village, Health
  Center) → Save → encounter-type select page → First/Subsequent encounter.
- Prenatal Laboratory point-of-care blood sugar: Laboratory activity → "Random Blood Sugar"
  tab (right end of the tab strip) → performed today Yes → Point of Care → before meal → the
  mg/dL input appears.
- NCD Laboratory opens directly on Random Blood Sugar; extra question "Did you perform this
  test today?" before the input appears.
- Labs history: subsequent prenatal encounter → Laboratory shows ONLY the History task while
  labs from previous encounters are pending → each row's UPDATE opens
  `#prenatal-labs-history/...` with that test's result form.
- Same-day second encounter is impossible; backdate the first via drush
  (`field_scheduled_date` value+value2, see e2e `backdateEncounter`) — no client resync
  needed when the client re-downloads afterwards (fresh device) or syncs before starting.
- Case Management (nurse): All / Contact Tracing / ANC Labs / NCD Labs panes; the forward
  icon on an ANC/NCD Labs entry opens the recurrent Lab Results page directly. Lab tech
  (PIN 3333) sees only Case Management + Device Status, ANC Labs pane only.

## Navigation facts (discovered 2026-08-21)

- Nurse **Maya**, PIN 1234, is offered two health centres after sign-in (Muhondo /
  Nyange); the QA device is set up for **Nyange**. Signing out returns to the PIN page
  without unpairing the device.
- Demo children are all born in 2022 (~4 years old): fine for height/weight/MUAC, but
  too old for head circumference or the newborn exam (pregnancy summary). Those need a
  child registered for the purpose.
- Well Child Nutrition Assessment task strip: Height / MUAC / Nutrition / Weight, each
  its own Save. Rwanda ranges shown above the inputs: height 25–250 cm, MUAC 5–99 cm,
  weight 0.5–200 kg.
- ANC Examination task strip: Vitals / Nutrition Assessment / Core Physical Exam /
  Obstetrical Exam / Breast Exam. Height, weight, BMI and MUAC sit on **one** task
  (so one Save covers all three ranged measurements); fundal height is on Obstetrical
  Exam behind "Is fundal palpable? → Yes". Neither form prints its range above the
  input — only the warning states it.
- The out-of-range warning draws a full-page dimmer: the task strip, the back arrow and
  everything else in the app are unclickable while it is up. Its Close button is the
  only in-app way out; the browser's Back button goes around it.
- **Group sessions start empty on this device**: every group reached from Clinical →
  Group Encounter reports "This Group has no mothers assigned to it", although the
  backend has `pmtct_participant` rows for those clinics (Nyagne II nid 93 has 26).
  Build the group instead: Attendance → **Add New Participant** → register the mother →
  on her Person page **Add Child** → register the child → pick "is the parent of" →
  Save. That creates the participant in *this session's* group and checks the mother in;
  the child's activities are then reached from the PARTICIPANTS icon in the header →
  the mother's card → the baby icon beside her photo.

## Roles (discovered 2026-08-21)

- Nurse PIN **1234** ("Maya") → pick a health centre. CHW PIN **2345** ("Jojo") → pick a
  village (Akanduga at Mbirima / Busake at Busake). Lab tech PIN 3333.
- The CHW's individual encounter list is larger than the nurse's: Acute Illness,
  Antenatal Care, Child Nutrition, **Well Child Visit**, **Child Scorecard**,
  TB Management, HIV Management. The nurse gets Acute Illness, Antenatal Care, Child
  Nutrition, Noncommunicable Diseases, Standard Pediatric Visit.
- **The newborn exam (Birth History / pregnancy summary) is CHW-only.** A nurse always
  starts a `PediatricCare` encounter, and that encounter type never offers the activity
  (`Pages/WellChild/Participant/View.elm` — `NewbornExam` is chosen only when `isChw`
  and the child is under two months). Register the newborn under the CHW login.
- A CHW's group session opens straight onto Attendance (one group, no programme or
  group choice); the nurse goes Clinical → Group Encounter → programme → group.

## Registration forms (2026-08-21)

- The CHW registration form has **no address section** — only names, DOB, gender, mode
  of delivery (children), and for adults education and marital status. The nurse's form
  adds the Province→District→Sector→Cell→Village cascade and Registering Health Center.
- Fields appear and disappear as the DOB is set: "Mode of delivery" is asked only once
  the date makes the person a child, "Level of Education" / "Marital Status" only once
  it makes them an adult. Set the DOB before filling the rest.
- The DOB picker is two selects plus a day grid and its own SAVE button. Changing YEAR
  rebuilds the MONTH list.

## Browser environment the tools give us (measured 2026-08-21)

- Tabs driven through the Chrome tools run under a fixed emulated viewport:
  `innerWidth/innerHeight` = **1200 x 1799**, `devicePixelRatio` 1, `outerWidth/outerHeight`
  0 (the signature of a device-metrics override), `ontouchstart` false. `resize_window`
  resizes the OS window but does not change the viewport, so device emulation — iPad Mini or
  anything else — is not reachable, and neither are touch-only interactions.
- Screenshots come back scaled (~900 px wide) from that 1200 px viewport, and the scale
  shifts when the window is resized. Coordinates are only valid for the screenshot they were
  read from: re-screenshot after anything that changes the layout, or click by `ref`.
- The app's own `<meta name="viewport" content="width=800">` has no effect at this width.

## What each browser tool costs — and why the window must be visible (measured 2026-08-21)

Timed from inside the page (`Date.now()` either side of each action, all in one batch so no
model latency is included), the same actions with the Chrome window hidden and then visible:

| action | tab hidden | window visible | ratio |
|---|---|---|---|
| `computer left_click` | ~5000 ms | **54–63 ms** | ~80x |
| `computer hover` | 5007 / 5007 / 5004 ms | **206 / 216 / 218 ms** | ~24x |
| `computer screenshot` | 5199 ms | **143 / 211 ms** | ~30x |
| `find` | 1128 ms | 1221 ms | unchanged |
| `javascript_tool` | 1 ms | 1 ms | unchanged |
| `computer wait(n)` | exactly n s | exactly n s | unchanged |

**So: raise the Chrome window before the run and keep it in front.** Hidden, every click,
hover and screenshot costs a flat five seconds — a wait for a paint that never comes, because
a hidden tab stops requestAnimationFrame. The pairing scenario spent ~95 s on 19 such actions
that would have cost about 4 s with the window up. Check it at the start of a run and say so
if it is wrong:

```
javascript_tool: JSON.stringify({hidden: document.hidden, focus: document.hasFocus()})
```

Even with the window visible, the costs that remain shape how a run should be written:

- `find` is ~1.2 s and `javascript_tool` is ~1 ms. **Read state with JS, not with `find` or a
  screenshot.** Screenshot when the frame is wanted as evidence, not to see what happened.
- Each separate tool call also costs the model's own turn — around 9 s in this session — so
  put as many actions as possible in one `browser_batch`.
- Deliberate `wait`s for sync are honest waits and cannot be optimised away; they were ~45 s
  of the pairing run and will dominate once the 5 s tax is gone.
