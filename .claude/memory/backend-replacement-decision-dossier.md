---
name: backend-replacement-decision-dossier
description: "Consolidated decision dossier (2026-10-11) for replacing the Drupal 7 backend — every verified fact from the discovery (contract, data model, scale, server logic, job layer, Jenkins), every candidate assessed (IHP, Acadia, Django, Phoenix, Laravel, Drupal 10), the envelope-schema position, recommendation, estimate and open questions"
metadata:
  node_type: memory
  type: project
  originSessionId: cea3b84a-2b0d-4b02-b4fa-f45be4798fb9
  modified: 2026-10-11T11:30:00.000Z
---

Everything the backend-replacement discovery established, in one place, so a go/no-go and a
stack choice can be made without re-surveying. Facts were verified 2026-10-10/11 against the
code, production SQL, the IHP PoC repo, acadia.engineering and ci.gizra.com. Detail files:
[[backend-replacement-discovery-2026-10]] (narrative), [[jenkins-scheduled-jobs-inventory]]
(job table), [[prod-advancedqueue-jenkins-serialized]], [[design-brief-backend-per-record-commit]],
[[deploy-tooling-and-aos-backend-state]].

## 1. Why, and how much time there is

- Drupal 7 core EOL 2025-01-05. The team considered Drupal 10/11 and rejected it. An IHP rewrite
  was started (`amitaibu/ihp-eheza`) and paused for lack of project certainty.
- Safety net today: the four Pantheon sites run **D7ES core 7.106 (2026-08-19)**, the Drupal 7
  Extended Support stream; 7.105 (2025-10-01) added PHP 8.4 support. ddev still pins PHP 7.4.
- D7ES covers core only. Not covered: RESTful 1.10, Advanced Queue, Features, Strongarm,
  restful_token_auth, Views/VBO, the SimpleTest harness. So the deadline is soft, the need is not.
- Pantheon hosts only Drupal/WordPress on MariaDB (all 4 sites listed as framework drupal).
  **Any non-Drupal backend means new hosting**, new backups, and a data-residency check for
  health data with TIP/partners.

## 2. What the backend really does (six jobs)

1. **Sync engine** for ~3,100 devices (the only thing devices talk to).
2. **Save-time rules**: uuid generation/immutability, shard assignment and re-sharding on
   health-center change, WHO z-scores on height/weight presave, soft-delete cascade,
   case-management follow-up resolution, person rules, village auto-creates its CHW clinic,
   nurse PIN handling, incident recovery, delete/unpublish forbidden unless super-user mode.
3. **Derived data / reports**: `hedley_reports` (~10.9k lines; Completion rules ≈7k lines
   re-implement client logic; per-person Statistical Queries JSON in `person.field_reports_data`;
   large-scope `report_data` nodes), `hedley_stats` (~4k lines raw SQL per HC, cached, served via
   `/api/sync/<hc>?statistics=1`), `hedley_ncda`, CSV export (`hedley_view_export`), weekly text
   reports by email, admin Elm apps (reports / completion / scoreboard) over `/api/reports-data`.
4. **Staff admin**: Drupal admin Views + VBO on nurses, devices, health centers, villages,
   persons; pairing-code reset; feature flags (`hedley_admin_feature_<name>_enabled`, drush vset);
   `hedley_super_user_mode`.
5. **Auth and files**: pairing code → token; token-authed image styles; photo upload.
6. **Operations**: Advanced Queue + Jenkins scripts (section 6), WhatsApp via Twilio
   (`hedley_whatsapp`), SMTP Mandrill, GlitchTip (sentry sdk), Robo deploy to Pantheon,
   8 SimpleTest classes.

## 3. The device contract (what must be byte-compatible or versioned)

| Endpoint | Notes |
|---|---|
| `GET /api/sync?base_revision=&db_version=` | "general" shard, 8 types (catalogue, health centers, nurses, villages…) |
| `GET /api/sync/<hc-uuid>?base_revision=&db_version=` | "authority" shard, 205 types; `statistics=1` returns cached HC dashboard stats |
| `POST /api/sync` / `POST /api/sync/<hc-uuid>` | `{changes:[{uuid,type,method POST\|PATCH,data}]}`, 50 per batch, one transaction |
| `GET /api/pairing-code/<8 digits>` | one-shot; returns a 5-year `restful_token_auth` token, sent later as `?access_token=` |
| `POST /api/file-upload`, `/api/bulk-photos` | photos (binary container) |
| `POST /api/report-state`, `/report-incident-details`, `/report-sync-incident` | client telemetry |
| `/system/files/styles/person-photo/…` | token-authed image styles |
| `/api/reports-data?app_type=reports\|completion\|scoreboard` | cookie-authed, admin Elm apps in `server/elm` |

- Cursor = `node.vid`; every save forces a new revision (`hedley_device_node_presave`), so the
  revision id is a global, monotonic sequence. Download batches of 500 ordered by vid.
- Shard = `field_shards` (health-center refs). POST = idempotent upsert by uuid; no DELETE
  (soft delete via `field_deleted`). Entity references travel as uuids, stored as nids;
  unknown uuid → `sync_incident` node + email ("Could not find UUID"). `db_version` must be 32.
- Known design gaps to fix in a rewrite: any device token can pull **any** HC shard; nurse PINs
  are synced in plaintext and checked client-side; whole-batch failure on one bad item (see
  [[design-brief-backend-per-record-commit]]).

## 4. Data model and scale

| Measure | Value |
|---|---|
| Node bundles | 219 (≈215 in use) |
| Field bases / instances | 400 / 2,149 |
| `list_text` enum lists | 208 (127 multi-valued) |
| Unlimited-cardinality fields | 137 |
| Entityreference fields | 34 |
| Custom DB tables | 0 (nurses, devices, reports are all nodes) |
| Taxonomies | 1 (`eheza_geolocation`) |
| Strongarm variables | 1,904 |
| ihangane live | 5,124,636 nodes; 10,129,105 revisions; 443,225 files; 3,215 users; 3,096 devices; 51 HCs; 232,560 persons; 57,170 MB in 962 tables |

Code: ~199k PHP lines, ~141k generated (Features/strongarm/views exports) → **~58k hand-written**,
of which reports ≈11k, stats ≈4k, sync class ≈0.6k. Client: ~255k lines Elm, offline-first PWA.

## 5. Candidates assessed

| Candidate | Coverage verdict | Key facts |
|---|---|---|
| **IHP** (continue PoC) | ✗ restart, don't continue | PoC: 61 commits, feature work stopped 2025-07-06, last bump 2026-02-02; ≈3–5 % coverage (5 authority + 3 general download types, 2 upload types); one table per type, no `deleted`, `base_revision` ignored, `revision_count` always 0, no jobs, no sync tests, DB drop-and-recreate; client branch `ihp-poc` changed sync URLs. ⚠ `flake.nix`/`common.nix` commit plaintext RDS password, session secret, AWS key pairs — **rotate**. |
| **IHP** (restart on envelope) | ✓ runner-up | IHP v1.5.0 (2026-03-25, hasql, GHC 9.10), v1.6.0 (2026-06-21, routes DSL, worker split). Active, single vendor (digitally induced), 223 open issues. Jobs: table-backed, `runAt`, separate RunJobs binary, retries/backoff; dedup and periodic scheduling must be built. Type safety; Gizra Haskell investment. |
| **Django + Postgres** | ✓ recommended | Admin covers staff UI nearly free; lowest bus-factor risk; JSONB envelope natural. Queue: **Procrastinate** (Postgres-only; `queueing_lock` = exact "one queued per id" semantic; periodic tasks; retries) rather than Celery. Django 6.0 `django.tasks` is an interface without a production backend. |
| **Phoenix/Elixir** | ✓ best runtime, worst team fit | Oban (unique jobs, cron plugin) is the strongest job story of all candidates. Only if the team wants Elixir. |
| **Laravel + Filament** | ✓ viable if staying PHP is acceptable | Queues + `ShouldBeUnique` + scheduler cover the job layer; Filament covers admin. |
| **Acadia** (acadia.engineering) | ✗ not now | Evan Czaplicki's Elm-like DB language; alpha 0.3.1 (2026-08-18), closed-source binary, Acadia Engineering ApS (DK). No commercial licence exists (Unregistered/Personal only, data-access-on-lapse clause); SQLite only public; no List/array, Bytes, Date, JSON types; no window functions; no jobs; no admin UI; Elm 0.19.3 (we pin 0.19.2). Revisit when commercial licence + Postgres ship. |
| **Drupal 10/11** | ✗ (team decision; agreed) | Migration of 219 bundles/2,149 instances is a rewrite anyway, keeps the generated-config weight. |
| Postgres-centric (PostgREST/Hasura/Supabase) | ✗ | Save-time rules and reports need a real application layer. |

## 6. The job layer (first-class, per user 2026-10-10)

**Advanced Queue today** — 4 queues: stats (`hedley_stats.module:589`), reports
(`hedley_reports.module:176`), ncda (`hedley_ncda.module:132`; retry after 120 s, max 3 attempts),
view export (`hedley_view_export.module:44`; retry 10 s, max 1000 attempts, lease 120 s, 500 rows
per pass, self-requeueing). Dedup helper `hedley_general_add_task_to_advanced_queue_by_id` refuses a
task whose title `<queue>_<id>` is already QUEUED, or PROCESSING < 20 min
(`hedley_aq_task_max_processing_period`). Stats are enqueued from the request path on cache miss.
Drained every 10 min by Jenkins, one worker per site (`concurrentBuild=false`).

**Semantics a replacement must offer:** dedup by id while queued; retry with cap and lease;
periodic scheduling; long self-requeueing jobs; enqueue from the request path; job chaining
(phase 2 after phase 1). Mapping: Django→Procrastinate ✓; IHP→build dedup+cron; Phoenix→Oban ✓✓;
Laravel→built-in ✓; Acadia→nothing.

**Jenkins (ci.gizra.com) — all known, see [[jenkins-scheduled-jobs-inventory]]:** per live site
`H/10` queue drain (`--timeout=600`), `H 23` overnight (super-user on → duplicate sweeps + EDD
fill → off; 10 Completion generators `--exclude_set=1`; then large-dataset recalculation for
health-centers/provinces/global), `H/5` WhatsApp (ihangane only; som/uvl disabled), `H 2`
`terminus backup:create`, Thursday `H 1` weekly text reports by email; multidevs get dedup
nightly; nutrition multidev also NCDA + Completion large-dataset recalculation. Slack
`#sitehealth` on failure. Every job is a thin shell around a drush script → becomes a named
command + in-app scheduler; keep Jenkins as trigger during transition.

**Prior art found:** disabled job **"Data Pipeline"** clones live → `aos-backend` multidev, runs
`hedley_migrate/data-pipeline/scripts/export-delta.php --since-vid=<last> --site=rwanda`, loads
SQL into an external Postgres `encounters` DB (34.32.23.190:5432, mTLS, `sync_state.last_vid`,
MODE delta|full-seed). This is exactly the vid-cursor export the migration needs. The script is
**not** in develop, any pushed branch, local worktrees, or the Pantheon clone's `origin/aos-backend`.

## 7. Design position (stack-independent)

- **Choose the storage shape before the language.** Generic entity envelope: `uuid, type,
  revision (global sequence), shards[], person, encounter, nurse, date_measured, deleted,
  created/changed, data JSONB` + a file table; field registry generated from the Features exports.
- **Invisible cut-over:** keep uuids and vids, seed the revision sequence above max Drupal vid,
  keep existing tokens → no re-pairing, no client release required for the switch.
- **Drupal as oracle:** run the old and new `/api/sync` side by side and diff output per shard
  (differential tests) before each site cut-over.
- **Client field additions need no backend migration** with the envelope; new enum values still
  need the two-release rollout ([[enum-variant-breaks-deployed-clients]]).
- **Completion rules:** run the client's compiled Elm Completion logic in a Node worker instead of
  re-implementing ~7k lines of PHP.
- **Fix while rewriting:** per-device shard authorisation; PIN hashing; hard-delete only inside
  the dedup sweep (today a global super-user toggle is flipped nightly); report sender/recipient
  into configuration (today a personal address + a Drupal variable); per-record commit.

## 8. Recommendation, estimate, open questions

- **Recommend:** Django on Postgres with the envelope schema and Procrastinate. **Runner-up:** IHP
  restarted on the envelope (not continued from the PoC). Phoenix only if the team wants Elixir;
  Laravel+Filament if staying PHP is acceptable. Not Acadia, not Drupal 10, not Postgres-only.
- **Estimate:** a quarter to half a year for one experienced developer with agents before the
  first site cut-over; sites then cut over one at a time (vhw/uvl smallest first).
- **Open questions for the user/team:** where does `export-delta.php` live and what is the
  external `encounters` Postgres for (precursor of the migration? analytics?); hosting target and
  data residency for TIP/Rwanda/Somalia; whether Jenkins stays as the scheduler; rotate the
  secrets committed in `ihp-eheza`.

**How to apply:** when the user returns to this topic, start from this dossier and the decision
they make, not from re-surveying. Re-verify only the moving targets: D7ES version, IHP release,
Acadia licence/backend status, and whether the Data Pipeline script has been located.
