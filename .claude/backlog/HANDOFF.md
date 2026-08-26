---
name: session-handoff
description: Live cursor for the E-Heza improvement work — read FIRST when resuming; rewritten 2026-08-24, R25 added 2026-08-25, B-235 shipped 2026-08-26
metadata: 
  node_type: memory
  type: project
  originSessionId: d78d3330-6ce4-4b84-aa0a-57da7f422346
  modified: 2026-08-26
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

## Open right now — ONE PR

| PR | branch | item | CI |
|---|---|---|---|
| #2150 | `b235-healthy-start-gwg-inverted` | B-235 | **10/10 green**, reviewed, awaiting the user's merge |

Everything else of mine has merged: **#2134 (B-194)** and **#2136 (B-189)** went in on 2026-08-25,
the four that were red on the 2026-08-17 GitHub incident (#2090, #2095, #2097, #2099), and the whole
R22 stack (#2108…#2116). The GitHub incident is over and was never a real signal — all of those went
green on rerun.

## Round 25 ran 2026-08-25 — 33 new items, THREE tier 2, all three LIVE

B-235..B-267 (see `rounds.md` R25). A 9-unit coverage sweep over the surfaces R24 named un-rowed
plus the biggest thematic-only files. **Tier 2 gained three, and every one is deployed:**

- **B-242** — the dashboard's "Current Pregnancies" / "Mothers in ANC" card has its date operands
  swapped, so it counts only pregnancies registered **on the reference day** (which is *today* for
  the default view) instead of every pregnancy open that month. Live since `a07da69ea`, 2023-12-28.
- **B-244** — editing a person whose photo was not re-taken sends `"photo": null` and the backend
  **deletes the stored photo**. The exact shape B-150 fixed for GPS, on a key that fix did not cover;
  a parent's address edit propagates to children, so it can wipe theirs too.
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

⚠ **B-248 contradicts something already written in this backlog.** Three `HedleyStatsCalculation`
scenarios — including the B-032 and B-033 soft-delete regression tests — have exited early without
running since a 2020 commit titled *"Make Travis pass"*, and CI has reported them green ever since.
Both of those entries claim "CI green … new scenario ran end-to-end". That claim does not hold.
**A passing CI job is not evidence a scenario ran.**

Three sizing queries are still written into their entries for the user to run: B-244 (photo
revisions vs current), B-245 (the 15 diagnosis values), B-266 (duplicate whatsapp_records). B-235's
has been answered.

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
the server scorecard) **and B-220** (Acute Illness progress report never renders the encounter it was
opened for — regression from `04e4bd9a8`, v1.14.0; a first visit's report shows nothing). Both are on
`origin/main`. Tier 3 gained B-210/B-212 (prenatal report outside-care + NCD referral lines), B-214,
B-215 (adult Patient Record never fetches acute-illness history), B-216 (adult DOB edit rewrites
children's enrolment dates server-side), B-217 (merge accepts original == duplicate), B-221 (partner
HIV test never triggers the prenatal assessment). Counts after R23: **124 READY — T2 4 · T3 19 · T4 67**.

## The queue, as of 2026-08-24 morning (pre-R23) — **109 READY**

| tier | READY | reality |
|---|---|---|
| **1** | 0 | empty |
| **2** | **1** | only **B-120**, and it needs a live query + a team answer before it is code. B-189 shipped as PR #2136 on 2026-08-24. |
| **3** | **13** | ~6 buildable; **6 of the 13 are one question away** (below) and 1 needs a user decision |
| **4** | **61** | the deep pool — unchanged in character |
| untiered | 34 | 26 = the never-started TH-track (test hardening) and G-track (CI guards); 8 = confirm-before-build |

### Tier 2 in full — one item left, and it is not pick-up-and-go

⬇ **B-157 was re-tiered 2 → 4 on 2026-08-24** (user: *"seems like infra issue. Why is it T2?"*). Its tier was inherited from part **(a)** — the only server-side test job going green with zero tests run — which shipped in PR #2067. The remaining **(e)** unpinned `npx elm-review` and **(f)** `test_shell.sh` coverage gap are CI hardening with a developer-only blast radius; they now sit in tier 4, next in spirit to the untiered G-track guard tooling. ⭐ **The general rule this produced: when the part that earned a split item's tier ships, RE-TIER THE REMAINDER.** Severity was dropped to MED-LOW the same morning without the tier following, which left a leftover being presented as the top of tier 2.

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
  **five** closures and ZERO live members. Repo-side liveness signals are worthless there — ask.
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
10. **Never touch the main working tree.** Worktree per item; release it the moment the PR is pushed.
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
- **Terminus works** for read-only live queries: `terminus drush <site>.live -- sqlq "…"`. Keep each
  query SIMPLE (the permission classifier blocks multi-subquery SELECTs); one subquery is fine.
  ⚠ **The auto-mode classifier can also block it outright** (it blocked B-120's 3-table join on
  2026-08-24) — hand the user the command rather than fighting it.
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
