---
name: session-handoff
description: Live cursor for the E-Heza improvement work — read FIRST when resuming; rewritten 2026-08-17
metadata: 
  node_type: memory
  type: project
  originSessionId: 5739f28b-8e7a-4b1c-bb51-d360c41af95c
  modified: 2026-08-19T11:36:27.463Z
---

# Session handoff — E-Heza improvement work

Rewritten **2026-08-17**, replacing the 2026-08-03 version, whose PR cursor stopped at #2030 and
whose queue picture is now wrong in both directions (Tier 1 and Tier 3 have emptied; Tier 4 has
not). Everything below is verified against GitHub, CircleCI and live databases on 2026-08-17, not
remembered.

⚠ Written as a **factual cursor**. The "Suggestions" section at the end is explicitly labelled and
is not a brief — a discovery session is expected to frame its own round (user, 2026-08-17).

## ⚠ NONE OF THIS IS LIVE YET

**`develop` is 327 commits ahead of `main`.** The last release merge into `main` is #1765; PR #1918
("Developments starting July 7, 2026") is the open develop→main release PR and holds **89 merged
PRs**. "Merged" anywhere in these notes means merged to `develop`, never deployed.

Consequences that keep catching people:

- Do not tell the user a fix protects live data until it ships. When reasoning about what damage a
  backfill must repair, the cutoff is the **deploy**, not the merge.
- **No release date is set** — "closer to September". So work that reduces the risk of that large,
  un-QA'd release is worth more than new surface.
- ⛔ **A symptom seen in live DATA must be diagnosed against the DEPLOYED code**, which is ~200+
  commits behind. `server/.pantheon-ihangane/` and `.pantheon-vhw/` are checkouts of what runs —
  **and the working tree of those checkouts can itself be months stale.** Read via
  `git show origin/master:<path>` there, and check `git log -1 origin/master` first. On 2026-08-17
  the working tree sat at 2026-05-11 while `origin/master` was 2026-07-07, and reading the files
  inverted a conclusion (B-170).

## Open right now — four PRs, all mine, all red for one external reason

| PR | branch | item | CI |
|---|---|---|---|
| #2099 | `b102-dead-sw-entity-row` | B-102 | 3 fail — **all package downloads** |
| #2097 | `b154-doses-past-the-schedule` | B-154 | 3 fail — same |
| #2095 | `b151-geo-generator-normalizes-paths` | B-151 | 3 e2e fail — **not the diff** (proven below) |
| #2090 | `b163-anchor-dashboard-months-on-generation` | B-163 | **10/10 green** |

⚠ **GitHub was in a major incident on 2026-08-17 from 13:40 UTC**, still open at 15:21: ~20% error
rate on web/API, **~50% on archive and raw content downloads**, later 429 rate-limiting, and Git
Operations degraded by 15:21. Every red job above is a failed `elm` package or composer zipball
fetch. **First action on resuming: check https://www.githubstatus.com/ , then rerun each of #2099,
#2097, #2095 ONCE.** Do not retrigger during an incident — retrying into a rate limit makes it
worse, and each attempt costs a ten-job pipeline.

For #2095 specifically, the e2e failures were shown not to come from its diff: no fixture or demo
CSV names any affected location, nothing in the e2e path calls the geo generators, and
`generate-data-for-all.php` has zero geo references. Rerun to confirm; do not "fix" it blind.

## Merged since the last handoff (2026-08-11 → 08-17)

#2065, #2067, #2068, #2071, #2073, #2075, #2077, #2079, #2081, #2084, #2086, #2088, #2091, #2093.
Together they closed B-152a, B-157(a–d), B-161, B-147, B-164, B-159, B-166, B-165, B-169, B-168's
diagnostic, B-100, B-043, plus two conventions PRs.

## The queue, as of 2026-08-17

| tier | open | reality |
|---|---|---|
| **1** | **0** | empty |
| **2** | 2 | **neither is an implementation task**: B-120 needs scoping, B-145 is a team decision on unsanitised DB clones |
| **3** | 2 | **B-155** (deferred past the release by the user), **B-162** (needs a user decision, adjacent to the #2065 narrowing). *B-143/B-144 show up in scans but are merged — they appear only inside B-146's parked note.* |
| **4** | ~41 | the only substantial pool left |

**So there is no buildable queue outside Tier 4.** Discovery is the useful mode again.

⚡ **ADDENDUM 2026-08-19 — R21 (data-integrity, B-192..B-200 + B-158-reopen decision pending) and R22 (duplication: B-201/B-202 fully IMPLEMENTED — 8 open PRs #2108..#2116, client independent + server stack #2109→#2110→#2115→#2116 bottom-up, ~−1,100 lines, all awaiting user review via `/code-review medium <branch>`) both ran to completion. See coverage-map R21/R22 sections.**
⚡ **ADDENDUM 2026-08-18 — R20 ran and REFILLED the queue; the table above is superseded.** 20 new
items B-172..B-191 (tier 2: B-188 dashboard ANC4 card counts no visits, B-189 ECD donut Behind =
not-assessed; tier 3: B-174/175 stats defects, B-178..B-183+B-186/187 completion drifts — the
completion batch is GATED on the user answering "is the Completion report used?"); TH/G tracks
scoped into a 9-PR implementation plan with 5 open user questions (Q1-Q5, in the backlog's "TH/G
tracks SCOPED" section); B-148(a) upgraded CONFIRMED; B-079 closed (fifth scripts-family closure),
B-112 superseded by #2095, B-170 loose end proven, geo regen cadence answered (release-driven).
Full detail: coverage map R20 section + backlog R20 entries.

Tier 4 groups, for orientation only: proven dead-code deletions (B-103…B-107, B-136, B-137 —
~3,100 lines, ⛔ B-136 has 5 documented false positives), the a11y markup batch (B-092…B-095, ~134
sites, zero logic risk), ~11 trivial one-liners, 4 latent-but-severe (B-078, B-099, B-112, B-139),
and ~6 that need a product answer.

## Closed / parked in the last two days — ⛔ do NOT re-pitch

- **B-171** ❌ user: *"Close this item. I see no value in it."* (dashboard "Last updated" reports
  serve time). All three fix shapes are covered by the refusal.
- **B-142** ❌ user: *"These scripts are not in use."* ⭐ **The `hedley_admin/scripts` declined class
  now has FOUR closures and ZERO live members. Repo-side liveness signals are worthless there** — my
  case was "live Robo wrappers + survived #2024's purge", and it was wrong. Ask the user.
- **B-038** 🅿 parked by user, no reason given.
- **B-170** ✅ closed — already fixed and deployed. ✅ Its "stale-build devices" inference was
  PROVEN 2026-08-17 (R20 wave 1) by error-string impossibility: `required "obstetric_history"`
  has zero hits on origin/main, so the deployed build cannot emit that decode error — every
  post-deploy occurrence is a pre-18-May build. Trickle stopped 2026-07-21. ⭐ Durable: Rollbar
  `code_version` is hardcoded `'1.0'` in ALL deployed builds (dynamic version = `05a5cc74b`,
  develop-only) — until the next release, discriminate builds by which code could produce the
  message, never by code_version.
- **B-028** ✅ closed as a **duplicate of B-159** — the same defect was found twice under two ids.
  📌 Before starting any Tier-4 item, grep the backlog for the SYMPTOM and the FUNCTION NAME, not
  just the item id.
- **B-079** — I believe this should close for the same reason as B-142 (it is gated on the liveness
  of the very demographics script the user just declared unused). Not yet actioned.
- **B-112** — may already be fixed by #2095, which replaced the geolocation term-builder's
  name-based identity with path-based identity. Check before queueing.

## HARD RULES

1. ⛔ **NEVER request a Copilot review without asking** (user, 2026-08-02). A blocked request posts a
   stub that reads as a clean pass — check the body. [[pr-first-review-workflow]]
2. ⛔ **NEVER auto-run `/code-review high`.** Ask the user to run `medium` with the branch named
   (`/code-review medium <branch>`), or it diffs against `develop` and sweeps the stack.
3. ⛔ **ASK FOR THE REVIEW — 100% of cases.** The message announcing a PR must carry the
   copy-pasteable command. A vague "want a review?" does not count.
4. ⛔ **Titles: `<Feature area>: <what was wrong>`** (user, 2026-08-16 — 100% rule, now in the repo's
   own `CLAUDE.md`). Naming the defect is necessary and NOT sufficient; the area says *where*. Areas
   in use: Dashboard stats, Child scoreboard, Patient search, Wellbeing, Sync, Vaccination,
   Measurements, Patient merge, Reports, CI, Conventions. All 89 release PRs + 82 issues were
   retitled to this on 2026-08-16. [[meaningful-issue-and-pr-titles]]
5. ⛔ **Bodies describe CURRENT STATE only** — no verification sections, no process narrative.
   ⚠ And **re-read the body after a rework**: #2090's body still claimed behaviour that a later
   revision had reverted, and Copilot caught it.
6. ⛔ **Duplication includes a repeated EXPRESSION**, not just copied blocks; a helper with one
   caller belongs in its `let`. In `CLAUDE.md` since 2026-08-16. [[code-duplication-rule]]
7. ⛔ **Do not let the review cycle grow the diff.** ⚠ The converse also bit: on #2093 I *declined*
   coverage as "needs new fixtures" and the user overruled it — **"needs new fixtures" is a cost,
   not a reason**, especially for the paths a PR exists to protect.
8. **Never quote `[ci skip]` in prose** — CircleCI matches the token anywhere in the message.
9. **Verify liveness before deleting or trusting a script** — and for `hedley_admin/scripts`, ask
   the user rather than inferring (see B-142 above).
10. **Never touch the main working tree.** Worktree per item; release it the moment the PR is pushed.

## ENVIRONMENT

- **Read CI from CircleCI, not `gh`, when GitHub is degraded.** `gh pr checks` returned HTTP 503 and
  my watch loops reported a false "settled" three times on 2026-08-17. Use
  `curl -s https://circleci.com/api/v1.1/project/github/TIP-Global-Health/eheza-app/tree/<branch>?limit=14`
  then walk `steps[].actions[].output_url` on a failing `build_num`. No token needed (public project);
  **no CircleCI token is available**, so rerun-from-failed is the user's click, not mine — and the
  workflow page may not even offer it (config was 2.0 until #2084).
- **CircleCI config is `version: 2.1` since #2084** (2.0 was end-of-life 2026-09-21). Pipeline URL
  shape: `app.circleci.com/pipelines/github/<org>/<repo>/<pipelineNumber>/workflows/<workflowId>` —
  the pipeline number is NOT optional.
- ⚠ **CI caches no packages.** Every Elm and composer job re-fetches from GitHub each run, so any
  GitHub hiccup reddens an unrelated PR. Six such failures on 2026-08-12/13 and more on 08-17.
  Caching `~/.elm` would delete the class. Related: B-157(e) is still open.
- **Terminus works** for read-only live queries: `terminus drush <site>.live -- sqlq "…"`. Keep each
  query SIMPLE (the permission classifier blocks multi-subquery SELECTs); one subquery is fine.
- **Live sites:** `ihangane` (rwanda), `vhw` (burundi), `tip-somalia`. ⚠ `eheza-site` is **Drupal
  11.4.4 / drush 13** — D7 idioms (`vget`, `sqlq`) fail there.
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
  `server/elm`, and CI never rebuilds or compares it.** Editing `server/elm/src` alone leaves the
  running server app unchanged. Rebuild with the command in `server/elm/elm-watch.sh`:
  `elm make src/Main.elm --output ../hedley/modules/custom/hedley_general/js/elm-main.js` (plain, NOT
  `--optimize`).
- **phpcs:** `~/.config/composer/vendor/bin/phpcs`, standards Drupal + DrupalPractice, with CI's list
  `--extensions=php,module,inc,install,test,profile,theme,js,css` (from `ci-scripts/test_coder.sh`).
  ⛔ `--extensions=inc/test` FATALS. **Baseline-diff the findings** — `HedleyWebTestBase.inc` carries
  11 pre-existing ones and phpcs is advisory (B-155).
- ⚠ **`server/.pantheon-*/` are untracked and NOT gitignored** — `git add -A` sweeps them in as
  embedded repos. Add paths explicitly and check the commit stat.
- **`gh pr edit` is broken here** — use `gh api repos/.../pulls/N -X PATCH`. Inline review comments
  need the FULL 40-char head SHA.
- **`Fixes #N` NEVER auto-closes** — the default branch is `main`, not `develop`. Close issues by
  hand. [[release-issue-reconciliation]]

## Suggestions — NOT a brief, and not a framing

Offered as observations from the 2026-08-16/17 implementation runs. A discovery round should decide
its own shape.

1. **One defect shape produced four of today's fixes**: a value the server sends that the client's
   decoder cannot parse, which fails the whole download batch and takes a health centre's dashboard
   with it. Instances: B-100 (a raw `FALSE` date), B-043 (a `GROUP_CONCAT` list where a scalar is
   decoded — three separate fields), B-170 (a missing key against a `required` decoder). The pattern
   generalises to *every* place a server field meets a strict decoder. ⭐ Note the asymmetry that
   makes it dangerous: `decodeWithFallback` tolerates a bad VALUE, `required` does not tolerate a
   missing KEY, and `optional` tolerates MISSING but never MALFORMED.
2. **`Maybe.withDefault` on a key parser converts a format change into silent data loss.**
   `legacyDictToDict (String.toInt >> Maybe.withDefault 1)` would have collapsed 12 months into one.
   Worth asking where else a parser defaults rather than fails.
3. **Committed generated artifacts drift silently.** `elm-main.js` is one; look for others that CI
   neither rebuilds nor compares.
4. **The two never-started parallel tracks remain**: TH-00…TH-19 (test hardening) and G-01…G-07
   (guard tooling). They fit the pre-release risk profile better than new surface, and TH-28-style
   gaps are real — `hedley_migrate` has no test class at all, and `hedley_stats` had no fixture for
   prenatal MUAC or NCD HIV tests until #2093 added them.
5. ✅ Both bookkeeping items were actioned in R20 wave 1 (2026-08-17): B-112 is SUPERSEDED by
   #2095 (path-key rewrite eliminates the stale-cache mechanism — close when #2095 merges), and
   B-079 is ❌ CLOSED (user 2026-08-17: "Those scripts are not in use anymore" — both demographics
   scripts incl. the HC variant; fifth `hedley_admin/scripts` closure).
