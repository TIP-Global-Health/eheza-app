---
name: session-handoff
description: Live cursor for the E-Heza improvement work — read FIRST when resuming; rewritten 2026-08-24, R25 added 2026-08-25, R26 added 2026-08-30 evening, R27 added 2026-09-01, open-PR section rewritten 2026-09-01 evening and extended 2026-09-02 with B-213 (five PRs open, reviewed, awaiting merge)
metadata: 
  node_type: memory
  type: project
  originSessionId: d78d3330-6ce4-4b84-aa0a-57da7f422346
  modified: 2026-09-02
---

# Session handoff — E-Heza improvement work

Rewritten **2026-08-24**, replacing the 2026-08-17 version, whose central banner ("NONE OF THIS IS
LIVE YET") and whose queue table are both now wrong. Everything below is verified against GitHub,
the Pantheon checkouts and the item files on 2026-08-24, not remembered.

⚠ Still a **factual cursor**. The "Suggestions" section at the end is explicitly labelled and is not
a brief — a discovery session frames its own round.

## 🚢 THE RELEASE SHIPPED — the old banner is retired

**v1.18.1 was cut 2026-08-23** (previous release v1.18.0, 2026-07-07). Its notes say *"Deployed in
production on August 23, 2026 (All Sites)"*, and all four Pantheon checkouts confirm it:

| site | Pantheon `origin/master` | when |
|---|---|---|
| ihangane (rwanda) | `a03f5086f` | 2026-08-23 15:40 |
| vhw (burundi) | `6d1de87d9` | 2026-08-23 15:02 |
| tip-somalia | `1d3f5a453` | 2026-08-23 14:42 |
| uvl | `c9dc649dd` | 2026-08-23 13:12 |

**`develop` is 37 commits ahead of `main`, not 327.** The open develop→main release PR is now
**#2128** ("Developments starting August 21, 2026"); #1918 merged.

What changes because of this:

- A fix discussed here **now protects live data** rather than waiting on a release. The 2026-08-17
  rule "do not tell the user a fix protects live data until it ships" no longer applies to anything
  merged before 2026-08-23.
- **A symptom seen in live data can again be diagnosed against near-current code.** The ~200-commit
  deployed-vs-develop gap is gone. `server/.pantheon-*/` remain the authority on what is *actually*
  running, and the `git show origin/master:<path>` habit is still the correct way to read them —
  but the answer will now usually match develop.
- **B-120's "write it after the release" gate is MET** (see below).
- **B-168's monitoring gate is MET** — its diagnostic is live (see below).

## Open right now — updated 2026-09-01 evening

**Four backlog PRs open, all reviewed, all awaiting the user's merge** (built this session after the
morning's four merged — #2178/#2172/#2174/#2176, issues closed, worktrees released):

| PR | issue | item | branch | state |
|---|---|---|---|---|
| #2180 | #2179 | **B-272** | `B-272-vitals-previous-reading` | green, reviewed CLEAN (zero findings) |
| #2183 | #2181+#2182 | **B-303+B-304** | `B-303-B-304-translate-corrections` | green, review done; ⚠ ONE OPEN THREAD: the altitude finding (dosage literals per branch/language in `TreatmentDetailsHypertension`) awaits a user decision — fold/file/drop |
| #2185 | #2184 | **B-280** | `B-280-subsequent-diagnosis-correction` | green, review done (fixture finding fixed; follow-up finding filed as B-306) |
| #2187 | #2186 | **B-299** | `B-299-village-stock-cache` | green, review done (placement finding fixed `91be8e22d`, recompute-cost finding ACCEPTED by user) |
| #2189 | #2188 | **B-213** | `B-213-scorecard-age-at-measurement` | **fully green** (all 10 checks incl. simpletest + 3 e2e), review done — ZERO correctness findings; its one quality finding (the two new helpers are structurally identical) DECLINED by the user (*"leave it"*), thread closed |

Worktrees held, to be removed **when their PR merges**: `ihangane-wt/{B-272-vitals-previous-reading, B-303-B-304-translate-corrections, B-280-subsequent-diagnosis-correction, B-299-village-stock-cache, B-213-scorecard-age-at-measurement}`.

⭐ Session lessons already in memory: `elm-test-cannot-import-backend-update` (two CI kills before
diagnosis). B-305 (tier 4) remains the recorded #2178 follow-up: re-count the vhw.live
DTP-standalone records on the release day that ships the fix. **B-306** (tier 4) filed from the
#2185 review: an AI follow-up keeps the diagnosis it was saved with.
**Counts after B-213 shipped (2026-09-02): 184 READY — T2 2 · T3 41 · T4 107 · untiered 34.** Tier 2 READY is now **B-195** (half on hold as #2156, ⛔ do not re-pitch) and **B-232**.

(Older non-backlog PRs — #1720/#1698/#1694/#1626/#1487/#1411/#924 — are external or DO-NOT-MERGE and stay out of this table.)

**#2156 (B-195 measurement guard) is still a DRAFT and still ON HOLD** by the user (2026-08-27,
*"too dangerous"*), with five unaddressed inline findings. ⛔ Do not re-pitch it.

### What the 2026-08-30 session shipped, and the four lessons worth carrying

Five items in one sitting, all tier 3/4 "easy win" shaped — the user asked for more of the
#2160/#2162 shape (single file, few lines, mechanically certain, no product question).

- **B-247** (#2164) — nutrition report dropped edema for Standard Pediatric Visits, so the same child
  was SAM at District scope and not-SAM at Sector scope. ⚠ `server/elm` changes **must ship the
  rebuilt committed bundle** (`hedley_general/js/elm-main.js`); CI compares it byte-for-byte.
- **B-212** (#2166) — the progress report's NCD referral line was gated on the ARV enrolment answer.
- **B-239** (#2168) — the Partner HIV follow-up form had no Save button, so that encounter could
  never be ended.
- **B-214 + B-253** (#2170, one PR) — two pages took the head of a UUID-ordered encounter list. Plus
  two user-approved follow-ups: the sorted prenatal accessor is renamed
  `getPrenatalEncountersForParticipantDesc` (it had shared a name with the raw one, which is
  *how B-253 happened*), and `Prenatal/Participant/View` now calls it instead of repeating its body.

⭐ **Four durable lessons:**
1. **An arbitrary `List.head` is not automatically a defect.** The B-214 review reported two more
   sites in `Measurement/Utils.elm` as the same bug; both were **refuted** — the vaccination fold is
   `assembled.measurements :: previousMeasurements`, and `generatePreviousMeasurements` excludes only
   the anchor, so the set is anchor-independent. Check what actually depends on the choice.
2. ⛔ **A stale `client/elm-stuff` makes `elm-review` invent an unused import.** It flagged
   `Backend.Entities` in a file that uses `PrenatalEncounterId`, on a commit CI had already passed.
   `rm -rf client/elm-stuff client/review/elm-stuff` and re-run before believing it. Now in memory.
3. ✅ **And elm-review still earns its place:** on B-253 it caught a real regression the change
   introduced (`List.head` on `List.map`) that would have turned CI red.
4. **GitHub refuses a file-level PR comment on a path outside the diff**
   (`pull_request_review_thread.path could not be resolved`) — fall back to a PR comment and say why.

⚠ Superseded by R26 the same evening — see the Round 26 section: **178 READY — T2 3 · T3 44 · T4 97 · untiered 34**. Pre-R26 counts: 159 READY —
T2 3 · T3 34 · T4 88 · untiered 34.** Tier 2 READY is B-195 (half on hold), B-213 and B-232.

Side-findings filed rather than folded in: **B-270** (same-milestone-window ECD status decided
arbitrarily) and **B-271** (same dose at two encounters — winning date depends on the anchor). Both
are the same `AssocList.fromList` last-wins class.

Earlier merges, for context: **#2150 (B-235)** and **#2146** (e2e progress-report coverage) merged
2026-08-26; **#2134 (B-194)** and **#2136 (B-189)** on 2026-08-25; the four red on the 2026-08-17
GitHub incident (#2090, #2095, #2097, #2099) and the whole R22 stack (#2108…#2116). That incident is
over and was never a real signal.

## Round 27 ran 2026-08-31 → 09-01 — 14 new items, ZERO tier 2, 6 tier 3; first-ever rows for Measurement/View and Translate

B-291..B-304 (see `rounds.md` R27, `queue.md` R27 line). A 7-unit coverage sweep: shared Measurement form builders (first row), vaccination engine internals, Pages/Nutrition at R26 depth, group-session flow (first re-read since R13), StockManagement, a first mechanical Translate objective-wrongness sweep (3,552+448 records), and the client Backend entity layer. All seven scouts were killed by the session cap on launch evening; six were resumed next morning via SendMessage with context intact. **Counts after R27: 188 READY — T2 3 · T3 46 · T4 105 · untiered 34.** Dry-stop counter: 0.

Headlines, all live-sized from this seat (12 terminus queries this round):
- **B-292** ⭐ DEPLOYED — the vendored AssocList's `merge` REVERSES per-vaccine dose order, and the immunisation form reads the FIRST dose as "last": interval gate + catch-up date range wrong for every child followed by BOTH WellChild and ChildScoreboard — **3,802 such children on ihangane** (0 on vhw/uvl/tip-somalia).
- **B-294** — the assessment stored on nutrition measurements goes stale (**269** live divergent encounters); `populate-nutrition-assessment.php` exists on the server solely to repair this field, and the client keeps re-creating what it backfills.
- **B-296** — **604** children enrolled under two adults in one clinic; group check-in reads an arbitrary head of that list, so a child brought by the caregiver can be unmeasurable for the whole session.
- **B-303/B-304** — 16 objectively wrong translations (the allowed exception classes only): a wrong Carvedilol dose (5.25 vs 6.25 mg), two EPDS screening options collapsed to one Kirundi string, a 5-seconds ECD threshold that reads 1 second in Somali, and a Burundi main-menu button overwritten with another feature's name.
- ⭐ **Four-site feature-flag map snapshotted** (in the coverage lessons): HC stock OFF everywhere, village stock = tip-somalia only, ncda/healthy_start/group_education = ihangane only. Size the flag before tiering a flag-gated finding.

## Round 26 ran 2026-08-30 (evening, after the five-PR session) — 19 new items, ZERO tier 2, 10 tier 3, three of them DEPLOYED

B-272..B-290 (see `rounds.md` R26, `queue.md` R26 line). An 8-unit coverage sweep giving WellChild, AcuteIllness and NCD the
unit-depth re-read that turned Prenatal's R14 "clean" into 19 items in R25, plus the FIRST coverage rows for
`Backend/Measurement/` (Model+Utils ✅ clean; Decoder+Encoder ◒). Run in two waves at the user's request (4 + 5; the
unit-7 scout died on an API rate limit and was rerun). **Counts after R26: 178 READY — T2 3 · T3 44 · T4 97 · untiered 34.**
Tier 2 READY is unchanged (B-195 half on hold, B-213, B-232). Dry-stop counter: 0.

**The three deployed ones, all sized on live:**
- **B-277** Burundi Well Child (+ Child Scoreboard twin): the DTP-booster Save reads `dtpForm` instead of `dtpStandaloneForm`,
  so the 18-month booster entry is never stored — `vhw.live` has **2 DTP-standalone records, both dose-less**. One word, two files.
- **B-290** ✅ IMPLEMENTED (issue #2173, PR #2174, open — parallel session, 2026-08-31) — all sites: the NCD social-history encoder writes `cigarettesPerWeek` under `beverages_per_week` (refactor slip
  `c9610431b`, 2024-01-18) — last differing pair on `ihangane.live` is 2023-09-13; **88 drinkers' counts lost** since. One word.
- **B-285** ✅ IMPLEMENTED (issue #2175, PR #2176, open, 2026-08-31) — all sites: a TB/HIV follow-up question's ENGLISH string was overwritten with Kirundi in `4a4e5e838` (2025-08-20).
  Two lines. (The "wrong language in field" exception to the translation-quality decline applies.)

**The tier-2-shaped one that stayed tier 3:** **B-286** — NCD hypertension staging includes the encounter's OWN just-written
stage in the history it compares against, so a Stage-3 patient with one sys<100 reading is stepped down TWICE in one visit
(vitals save → Stage2, the always-expected RBS save → Stage1), and a mistyped BP cannot be corrected downward. Deployed
since 2022 — but `ihangane.live` has 65 sys<100 NCD readings and **none in a patient with a prior Stage 2/3**, so it has never
fired. Promote if the correction ratchet alone is judged enough.

**Two families rather than many items:** the raw-form-handler shape (B-274 AI medication reason with a REAL consumer — the
referral gate; B-279, B-283 siblings; NCD siblings proven INERT) and the same-session Yes/No-with-reason refill (**B-281**, four
AI handlers + NCD referral + shared `SendToHC`/`HealthEducation` halves used by every program). Each is one idiom fix in shared
code; do not present them per program.

**Two things this round corrected in the record:** (1) B-272's "only Well Child's list is DESC" was wrong — NCD is DESC too,
same one-line fix, folded into B-272. (2) The handoff suggestion below that "`optional` tolerates MISSING but never MALFORMED"
is imprecise: pipeline 1.0.1's `optional` is `oneOf [ decoder, Decode.null fallback ]`, so a present `null` yields the
fallback; only a present NON-null value the inner decoder rejects fails. The whole `optional`-enum class in
`Backend/Measurement/Decoder.elm` is null-safe (unit 7, mechanically).

**One candidate refuted on live data** (symptoms_general period counters added after the bundle: 0 of 21,420 nodes lack them).
⚠ Framing slip: the plan named `Pages/WellChild/Utils.elm`, which does not exist; the scout audited `Encounter/Utils.elm`.

## Round 25 ran 2026-08-25 — 33 new items, THREE tier 2, all three LIVE

B-235..B-267 (see `rounds.md` R25). A 9-unit coverage sweep over the surfaces R24 named un-rowed
plus the biggest thematic-only files. **Tier 2 gained three, and every one is deployed:**

- **B-242** ✅ **SHIPPED 2026-08-26 — issue #2153, PR #2154 (open, awaiting review + merge).** The
  dashboard's "Current Pregnancies" / "Mothers in ANC" card had its date operands swapped, so it counted
  only pregnancies registered **on the reference day** (which is *today* for the default view) instead of
  every pregnancy open that month. Live since `a07da69ea`, 2023-12-28.
  ⭐ **The entry's "swap the operands" fix shape was wrong** — `withinOrBeforeSelectedMonth` already
  existed in the same file, exported and reached from eight other counts, so the filter calls it; two
  correct-but-hand-rolled copies of the same comparison were folded in at the user's request. The same
  helper feeds the "With Danger Signs" cards and the high-risk total, which were wrong the same way.
- **B-244** ✅ **SHIPPED 2026-08-26 — issue #2151, PR #2152 (open, awaiting review + merge).** Editing a
  person whose photo was not re-taken sent `"photo": null` and the backend **deleted the stored photo**.
  The exact shape B-150 fixed for GPS, on a key that fix did not cover; a parent's address edit
  propagates to children, so it took theirs too. Live measurement: **3,215 persons already wiped**
  (ihangane 3,164 · vhw 50 · uvl 1 · tip-somalia 0), every one still recoverable, and the repair script
  ships with the fix. ⭐ Fixing the SHARED encoder helper rather than the person branch also closed the
  entry's untraced tail (the five photo measurements + the stock-update signature) — and exposed that
  the person branch had been a hand-copy of `doEncode`, so it collapsed into that call.
  ⚠ The backend guard is person-only: a measurement PATCH with a null photo is protected by the client
  fix alone. Reviewed with **no findings**; its one observation — no server-side coverage of the guard —
  was answered by folding the scenario into an existing method (the B-032 pattern: no new install).
  ⚠ **The first attempt at that landed in a class that never runs** — `HedleyRestfulBulkPhotosTest`
  declares the group `Hedley restful` while the job runs `Hedley` — and was caught only by grepping the
  CI log for the new assertions. Moved to `HedleyPatientConsolidation` in `05e8b9c9b`; the dead group is
  filed as **B-269**. ⭐ **Grep the CI log for a new test's own assertion text before calling it covered.**
- **B-235** ✅ **SHIPPED 2026-08-25 — issue #2149, PR #2150 (open, green, awaiting merge).** The
  Healthy Start gestational-weight-gain verdict was inverted (a woman gaining too little told
  "Adequate"; weight loss the most "adequate" result), flipped deliberately by `45f215dbd`. The flag
  gate is **answered**: `hedley_admin_feature_healthy_start_enabled: 1` on `ihangane` (absent on the
  other three), so it was live clinical guidance, deployed since v1.17.0.
  ⭐ **The "needs a product answer" gate dissolved on reading issue #1604** — the spec states the rule
  outright, and it also makes a plain revert wrong: the pre-flip code used strict `>`, so an
  exactly-on-target gain read Inadequate. Its adjacent note is confirmed and split out as **B-268**
  (the progress-report chart classifies from the self-reported pre-pregnancy weight where the spec
  and the activity use the booking weight).

Tier 3 gained 13, including **B-239** (the Partner HIV follow-up form has no Save button, so that
encounter can never be ended), **B-245** (15 prenatal diagnoses unmapped → diagnosed encounters
counted as "No Prenatal Diagnosis"), **B-247** (edema dropped for well-child → the same child is
SAM at district scope and not-SAM at sector scope), **B-266** (WhatsApp uploads carry no UUID, so a
retried batch sends the patient the report twice) and **B-248** (below). Counts after R25:
**167 READY — T2 7 · T3 39 · T4 87 · untiered 34** (after B-235 shipped and B-268 was
split out of it). Dry-stop counter: 0.
⚠ Superseded 2026-08-26: **B-220 was already shipped by the user as PR #2144** (issue #2143, merged
2026-08-25), so the counts are **165 READY — T2 5 · T3 39 · T4 87 · untiered 34**. Tier 2 READY is
B-195, B-213 and B-232 only: **B-120 was 🅿 PARKED by the user on 2026-08-26** (*"18 overall is too
little to create a script for it"*) and **B-242 shipped the same day as PR #2154**, so tier 2 READY is
**3**. B-244 (PR #2152) is being carried by a PARALLEL session — do not touch it from this seat.

⚠ **B-248 contradicts something already written in this backlog.** Three `HedleyStatsCalculation`
scenarios — including the B-032 and B-033 soft-delete regression tests — have exited early without
running since a 2020 commit titled *"Make Travis pass"*, and CI has reported them green ever since.
Both of those entries claim "CI green … new scenario ran end-to-end". That claim does not hold.
**A passing CI job is not evidence a scenario ran.**

Two sizing queries are still open in their entries: B-245 (the 15 diagnosis values) and B-266
(duplicate whatsapp_records).
⭐ **2026-08-27: live sizing queries can be run from this seat.** `terminus` is authenticated
(`terminus remote:drush ihangane.live -- sql-query "..."`, verified 2026-08-27), so a read-only
query does not need to be handed to the user. The 2026-08-24 note that "the auto-mode classifier
blocked it" was about one specific call and had been read as a general block — it is not one. B-195's
gate was answered this way in minutes after being presented as unanswerable. B-235's and **B-244's** are answered — B-244 measured **3,215 persons
whose photo was wiped** (ihangane 3,164 · vhw 50 · uvl 1 · tip-somalia 0), every one recoverable
from the revision table.

## Round 24 ran 2026-08-24 (after R23) — 10 new items, 1 tier 2, 5 tier 3

B-225..B-234 (see `rounds.md` R24). **Tier 2 gained B-232** (CHW Case Management immunization pane
lists a child only on the day the follow-up was scheduled — a copy of the nutrition entry generator
whose `LT` test can never pass when the scheduling and acting encounter are both Well Child; long-standing,
ask whether the pane is relied on before building). Tier 3 gained B-226 (nurse Postpartum button opens a
second encounter over an open antenatal one), B-227 (prenatal lab-results Blood Smear row: predicate inverted
by `5d553d346`, never shows a result), B-230 (`gonorrhea-continued` encoded but undecodable → silently
dropped since 2022; the ONLY encoder⊄decoder gap in all 209 list fields), B-231 (TB pane drops the patient
when the TB item is newer than the AI TB-suspect item), B-233 (AI-TB / dummy-HIV entries create duplicate
participants). Tier 4: B-225 (report-state change-guard dead → 2 user_saves per sync), B-228, B-229, B-234.
B-036 extended (feeding-form derived signs), B-019 (parked) annotated. Counts after R24: **134 READY —
T2 5 · T3 24 · T4 71**. ⚠ Two sizing queries the classifier blocked are written out in B-227 and B-230
for the user to run on live. Dry-stop counter: 0.

## Round 23 ran 2026-08-24 (after the bookkeeping below) — 15 new items, 2 of them tier 2 and LIVE

B-210..B-224 (see `rounds.md` R23). **Tier 2 now holds B-213** (NCDA scorecard grades every past
height/weight at the child's age TODAY — earlier months turn red as the child grows; disagrees with
the server scorecard) **and B-220** ✅ **IMPLEMENTED 2026-08-25 by the user (issue #2143, PR #2144, merged) — not from
this seat** (Acute Illness progress report never rendered the encounter it was opened for — regression
from `04e4bd9a8`, v1.14.0; a first visit's report showed nothing). Both were on `origin/main`. Tier 3 gained B-210/B-212 (prenatal report outside-care + NCD referral lines), B-214,
B-215 (adult Patient Record never fetches acute-illness history), B-216 (adult DOB edit rewrites
children's enrolment dates server-side), B-217 (merge accepts original == duplicate), B-221 (partner
HIV test never triggers the prenatal assessment). Counts after R23: **124 READY — T2 4 · T3 19 · T4 67**.

## The queue, as of 2026-08-24 morning (pre-R23) — **109 READY**

| tier | READY | reality |
|---|---|---|
| **1** | 0 | empty |
| **2** | **0** | ⚠ superseded 2026-08-26: **B-120 is 🅿 PARKED** by the user; tier 2 READY is now **B-195/B-213/B-232** (B-220 shipped as PR #2144, B-242 as PR #2154, B-244 is a parallel session). ⏸ 2026-08-27: **B-195's measurement guard PR #2156 is ON HOLD by the user** — read the entry before touching it. |
| **3** | **13** | ~6 buildable; **6 of the 13 are one question away** (below) and 1 needs a user decision |
| **4** | **61** | the deep pool — unchanged in character |
| untiered | 34 | 26 = the never-started TH-track (test hardening) and G-track (CI guards); 8 = confirm-before-build |

### Tier 2 in full — one item left, and it is not pick-up-and-go

⬇ **B-157 was re-tiered 2 → 4 on 2026-08-24** (user: *"seems like infra issue. Why is it T2?"*). Its tier was inherited from part **(a)** — the only server-side test job going green with zero tests run — which shipped in PR #2067. The remaining **(e)** unpinned `npx elm-review` and **(f)** `test_shell.sh` coverage gap are CI hardening with a developer-only blast radius; they now sit in tier 4, next in spirit to the untiered G-track guard tooling. ⭐ **The general rule this produced: when the part that earned a split item's tier ships, RE-TIER THE REMAINDER.** Severity was dropped to MED-LOW the same morning without the tier following, which left a leftover being presented as the top of tier 2.

⏸ **B-195, measurement half, IS ON HOLD 2026-08-27** — issue #2155, PR #2156, now a **draft**.
The user, after the review: *"It seems to be dealing with core functionality, and I feel it's too
dangerous."* Five findings are posted inline and unaddressed, the first being that the guard does
not close the race it was written for. It would have made saving a measurement again edit the stored
one instead of creating a second the app can never show. Three kinds of
record legitimately repeat and were exempted: family-nutrition per child, participant consent per
form, acute-illness trace contacts. ⚠ The same four rules are missing from
`hedley_admin/scripts/delete-duplicate-measurements.php`, which had already deleted a second child's
family-nutrition records on `tip-somalia.live` (encounters 1410 and 2402, five nodes, swept
2026-05-11). ✅ That script is **issue #2157, PR #2158**, shipped the same day.

✅ **B-189 shipped 2026-08-24** — issue #2135, PR #2136. The ECD donut now classifies from the most recent nurse encounter carrying an ECD verdict and puts seen-but-never-assessed children in a third grey **Not Assessed** slice, matching the rule the progress report already applied. Live: 56 genuine ECD warnings against ~21,000 no-verdict nurse encounters, so the red slice was ~0.25% real.

- **B-120** — backfill for relationships and group participations destroyed by past patient merges.
  ✅ **Unblocked by the release.** The scoping is done and the blocker it was filed with is solved:
  the merge map IS recoverable from the revision tables, and as of 2026-08-03 it was **18
  duplicate→original pairs, unambiguous, Rwanda-only** — only 18 of 159 soft-deleted persons were
  merge victims. Two things stand before code: **(1)** re-run the recovering query on
  `ihangane.live` to refresh those numbers (it is in the entry; read-only; needs the user to run it
  or approve terminus — the auto-mode classifier blocked it on 2026-08-24), and **(2)** a team
  answer that is not a code question: is un-deleting wanted in every case, or should a 2022 merge
  since corrected by hand be left alone? 18 people is small enough to eyeball a dry run.

B-145 and B-158 are 🅿 PARKED and stay out of the count.

### Tier 3 — what is buildable and what is gated

**Buildable today (5, + one half):** B-155 (retire the phpcs advisory workaround), B-175 (a person
with no gender makes the WHOLE health-centre stats payload undecodable — dashboards never load),
B-192 (hard-deleting an individual_participant silently drops its encounters from sync),
B-195 (create-button db-lag race mints duplicate active pregnancies), B-196 (participant EDD goes
stale when the LMP is re-dated — the B-140 dropped-patch mechanism, live data trail), and the
**backfill half** of B-198 (742 persons' group enrolments lost — its *writer* is parked as B-158,
but the data repair stands regardless).

**Gated — do not present as ready:** the six-item **Completion-report batch** (B-179, B-180, B-181,
B-183, B-186, B-187) all hang on ONE unanswered question — **is the Completion report actually
used?** — and **B-162** needs a user decision adjacent to the #2065 narrowing.

## MONITORING — one item, and its gate has just been met

**B-168** (the EDD trickle). It was waiting on the #2086 diagnostic reaching production. ✅ **That
happened**: `caa405b52` is in `origin/main` and the release deployed 2026-08-23. The instrumentation
is live, so the item is no longer waiting on us.

It now waits on **evidence**: the first `TriggerRollbar IndexedDB` event carrying an action label
(`set-edd-date`, `set-newborn`, or one of the four close actions) in the Rwanda Rollbar project. Its
cache-state string picks the branch — `failure` names the failing read, `loading` = lost reply,
`missing` = eviction, `not-asked` = never fetched. ⚠ Two lags before expecting anything: a device
only emits once it activates the new bundle (SW update, which these tablets rarely take promptly),
and the event rides the offline `dbErrors` lane that drains at the *next* sync. **~2 weeks with no
event is itself a result** — it would mean the no-op branch is not the drop site.

## Bookkeeping done 2026-08-24 — the index had been overstating the queue

Eleven entries were recorded READY while their PR was already merged, and six were mis-tiered.
Fixed in the entries (the index is derived, never hand-edited):

- **Marked IMPLEMENTED:** B-143 (#2055), B-144 (#2057), B-161 (#2071), B-159 (#2077), B-203 (#2118),
  B-074 (#1974), B-099 (#1977), B-134 (#2063), B-147 (#2073), B-164 (#2075), B-166 (#2079).
- **Re-tiered to match their own text:** B-189 → 2; B-190, B-191, B-193, B-197, B-199 → 4.
- **`reindex.py` fixed:** a bare `\bSTALE\b` was matching entries whose *title* contains the word —
  B-166 ("reschedules from the STALE timestamp") and G-03 ("Prune 4 STALE waivers") were indexed as
  lifecycle-STALE while actually implemented / READY. STALE now only matches in a lifecycle
  position. **No item in the backlog is genuinely STALE.**

📌 **The lesson, worth keeping:** the tier lines in `queue.md` recorded these PRs correctly while the
item entries did not, and the index derives from the entries — so the queue looked ~11 items deeper
than it was. **Record the entry status at merge time, not just the tier line** (skill step 5 exists
for exactly this).

## Closed / parked recently — ⛔ do NOT re-pitch

- **B-171** ❌ user: *"Close this item. I see no value in it."* All three fix shapes are covered.
- **B-142** ❌ user: *"These scripts are not in use."* ⭐ The `hedley_admin/scripts` declined class has
  **five** closures. Repo-side liveness signals are worthless there — ask.
  ⚠ **Corrected 2026-08-27: the class does NOT have zero live members.** The user named two that run
  **daily from Jenkins**: `delete-duplicate-measurements.php` and `delete-duplicates.php`. Asking is
  still the rule; "no live members" was an overreach from five individual closures.
- **B-079** ❌ user 2026-08-17: *"Those scripts are not in use anymore."*
- **B-038**, **B-158** 🅿 parked by user, no reason given.
- **B-028** ✅ closed as a duplicate of B-159 — the same defect found twice under two ids.
  📌 Before starting any item, grep the backlog for the SYMPTOM and the FUNCTION NAME, not the id.
- Full parked list (11): B-019, B-020, B-027, B-038, B-049, B-065, B-087, B-108, B-145, B-146, B-158.

## HARD RULES

1. ⛔ **NEVER request a Copilot review without asking** (user, 2026-08-02). A blocked request posts a
   stub that reads as a clean pass — check the body. [[pr-first-review-workflow]]
2. ⛔ **NEVER auto-run `/code-review high`.** Ask the user to run `medium` with the branch named
   (`/code-review medium <branch>`), or it diffs against `develop` and sweeps the stack.
3. ⛔ **ASK FOR THE REVIEW — 100% of cases.** The message announcing a PR must carry the
   copy-pasteable command. A vague "want a review?" does not count.
4. ⛔ **Titles: `<Feature area>: <what was wrong>`** (100% rule, in the repo's own `CLAUDE.md`).
   Naming the defect is necessary and NOT sufficient; the area says *where*. [[meaningful-issue-and-pr-titles]]
5. ⛔ **Bodies describe CURRENT STATE only** — no verification sections, no process narrative.
   ⚠ And **re-read the body after a rework**: #2090's body still claimed behaviour a later revision
   had reverted, and Copilot caught it.
6. ⛔ **Duplication includes a repeated EXPRESSION**, not just copied blocks; a helper with one caller
   belongs in its `let`. [[code-duplication-rule]]
7. ⛔ **Do not let the review cycle grow the diff.** ⚠ The converse also bit: on #2093 I *declined*
   coverage as "needs new fixtures" and the user overruled it — **"needs new fixtures" is a cost,
   not a reason**, especially for the paths a PR exists to protect.
8. **Never quote `[ci skip]` in prose** — CircleCI matches the token anywhere in the message.
9. **Verify liveness before deleting or trusting a script** — and for `hedley_admin/scripts`, ask the
   user rather than inferring.
10. **Never touch the main working tree.** It is parked on `develop` permanently (2026-08-27) so
    parallel sessions can each hold their own branch. One worktree per item at
    `/var/www/html/ihangane-wt/<id>`, created to work the branch and **released once the work is
    pushed**; recreate it if review brings more work. `git worktree list` is the claim
    board — check it before starting an item.
    (The `.claude/backlog/` files are the one exception — bookkeeping is edited in place.)
11. **Record the entry status the moment a PR merges** — see the bookkeeping note above.
12. ⛔ **Read the backlog from `develop`, and check the main tree's branch first.** The files are
    read from whatever branch `/var/www/html/ihangane` happens to be on, and the `Stop` hook only
    commits them on `develop` — so a main tree parked on a feature branch serves a **stale queue with
    no warning**. On 2026-08-26 it was two commits behind and B-235 was surveyed to the user as READY
    and flag-gated when it had shipped the day before. Start every session with
    `git log --oneline HEAD..origin/develop -- .claude/backlog/`; if it is non-empty, read the files
    from `origin/develop` (`git show origin/develop:<path>`) or in a `develop` worktree, and do the
    bookkeeping there rather than switching the main tree.

## ENVIRONMENT

- **Read CI from CircleCI, not `gh`, when GitHub is degraded.** `gh pr checks` returned HTTP 503 and
  watch loops reported a false "settled" three times on 2026-08-17. Use
  `curl -s https://circleci.com/api/v1.1/project/github/TIP-Global-Health/eheza-app/tree/<branch>?limit=14`
  then walk `steps[].actions[].output_url` on a failing `build_num`. No token needed (public project);
  **no CircleCI token is available**, so rerun-from-failed is the user's click, not mine.
- **CircleCI config is `version: 2.1` since #2084** (2.0 was end-of-life 2026-09-21). Pipeline URL
  shape: `app.circleci.com/pipelines/github/<org>/<repo>/<pipelineNumber>/workflows/<workflowId>` —
  the pipeline number is NOT optional.
- ⚠ **CI caches no packages.** Every Elm and composer job re-fetches from GitHub each run, so any
  GitHub hiccup reddens an unrelated PR. Caching `~/.elm` would delete the class, and pairs naturally
  with B-157(e).
- **Terminus live queries are ALLOWED as of 2026-08-26.** `.claude/settings.local.json` carries four
  rules — `Bash(terminus drush <site>.live -- sqlq *)` for `ihangane`, `vhw`, `tip-somalia`, `uvl` — so
  read-only live queries run without a prompt, joins included. ⚠ The rule is **station-local**
  (`settings.local.json` is gitignored), so a new station re-adds it; the classifier blocks
  `terminus drush … sqlq` outright without it, at any query complexity — it refuses the command
  SHAPE, not the SQL, so a bare `COUNT(*)` is blocked just the same. The rule is prefix-matched and
  therefore does not distinguish SELECT from a write; treat that as a rule of conduct, not a guard.
- **Live sites:** `ihangane` (rwanda), `vhw` (burundi), `tip-somalia`, `uvl`. ⚠ `eheza-site` is
  **Drupal 11.4.4 / drush 13** — D7 idioms (`vget`, `sqlq`) fail there.
- **Elm: ⛔ ALWAYS 0.19.2.** Host global `elm` is 0.19.2; use project-local
  `client/node_modules/.bin/elm-test` (global is 0.19.1 and silently rewrites `elm.json`).
- **Client worktree recipe** (two failed runs before getting this right): copy `client/src/generated`
  (gitignored — else `elm make` dies with MISSING SOURCE DIRECTORY) and `client/src/elm/LocalConfig.elm`;
  symlink `node_modules` (fine); give the worktree its OWN real `elm-stuff` — a symlinked one makes
  **both** `elm-test` and `elm-review` vacuous.
- **elm-test needs the CI glob**: `elm-test "src/elm/**/Test.elm"` — there is no `tests/` dir.
- **Full type-check = `elm make src/elm/Main.elm`** (548 modules). `server/elm` = `elm make src/Main.elm`
  (84 modules) and has its own elm-review.
- ⛔ **elm-review the COMMITTED state, in a fresh clone, before pushing Elm.** It caught an unused
  parameter on #2090 that compile + 2,974 tests both missed. Third time it has paid for itself.
- ⚠ **`server/hedley/modules/custom/hedley_general/js/elm-main.js` is a COMMITTED bundle built from
  `server/elm`.** ✅ CI now rebuilds and `cmp`s it (`ci-scripts/test_server_elm_bundle.sh`, added by
  PR #2118 as B-157(g)) — but editing `server/elm/src` alone still leaves the running server app
  unchanged, so rebuild with `elm make src/Main.elm --output ../hedley/modules/custom/hedley_general/js/elm-main.js`
  (plain, NOT `--optimize`) or the new gate turns the PR red.
- **phpcs:** `~/.config/composer/vendor/bin/phpcs`, standards Drupal + DrupalPractice, with CI's list
  `--extensions=php,module,inc,install,test,profile,theme,js,css`. ⛔ `--extensions=inc/test` FATALS.
  **Baseline-diff the findings** — `HedleyWebTestBase.inc` carries 11 pre-existing ones and phpcs is
  advisory (that is B-155).
- ⚠ **`server/.pantheon-*/` are untracked and NOT gitignored** — `git add -A` sweeps them in as
  embedded repos. Add paths explicitly and check the commit stat.
- **`gh pr edit` is broken here** — use `gh api repos/.../pulls/N -X PATCH`. Inline review comments
  need the FULL 40-char head SHA.
- **`Fixes #N` NEVER auto-closes** — the default branch is `main`, not `develop`. Close issues by
  hand. [[release-issue-reconciliation]]

## Suggestions — NOT a brief, and not a framing

1. **The release just went out un-QA'd at scale**, and the two parallel tracks that address exactly
   that risk (TH-00…TH-27 test hardening, G-01…G-07 CI guards) have still never been started. They
   are 26 of the 34 untiered READY items.
2. **One defect shape produced four fixes in August**: a value the server sends that the client's
   decoder cannot parse, which fails the whole download batch and takes a health centre's dashboard
   with it (B-100, B-043, B-170 — and B-175 is the same shape, still open). ⭐ The asymmetry that
   makes it dangerous: `decodeWithFallback` tolerates a bad VALUE, `required` does not tolerate a
   missing KEY, and `optional` tolerates MISSING but never MALFORMED.
3. **`Maybe.withDefault` on a key parser converts a format change into silent data loss** — worth
   asking where else a parser defaults rather than fails (that is B-172, still open).
4. **Committed generated artifacts drift silently.** `elm-main.js` is now gated; look for others that
   CI neither rebuilds nor compares.
5. **The R21 live-data items now describe production behaviour**, not something waiting to ship —
   B-192, B-195, B-196, B-198, B-199 all came with live counts. That is a different argument for them
   than it was a week ago.
