---
name: backend-replacement-discovery-2026-10
description: "Drupal 7 is EOL; discovery run 2026-10-10 on replacing the backend (IHP PoC state, inventory of what the backend really does, candidate stacks, envelope-schema idea)"
metadata:
  node_type: memory
  type: project
  originSessionId: cea3b84a-2b0d-4b02-b4fa-f45be4798fb9
  modified: 2026-10-10T13:03:46.832Z
---

Discovery run 2026-10-10 on replacing the Drupal 7 backend (D7 EOL 2025-01-05, PHP 7.4;
Drupal 10/11 was considered by the team and rejected).

**Facts gathered (verified that day):**
- Client-facing surface is tiny: `/api/sync` GET/POST (214 sync types through one handler,
  cursor = `node.vid`, shard = health-center uuid, uuid upserts, no DELETE), `/api/pairing-code`,
  `/api/file-upload`, `/api/bulk-photos`, 3 `/api/report-*`, token-authed `/system/files/styles/…`,
  plus cookie-authed `/api/reports-data` for the admin Elm apps in `server/elm`.
- Hand-written PHP ≈ 58k lines (199k total minus ≈141k generated Features/strongarm/views exports);
  the heavy parts are `hedley_reports` (10.9k, completion rules ≈7k re-implement client logic),
  `hedley_stats` (4k raw SQL), sync class 580 lines.
- ihangane live: 5.1M nodes / 10.1M revisions / 443k files / 57 GB / 962 tables / 3,096 devices.
- Pantheon hosts only Drupal/WordPress on MariaDB → any non-Drupal backend means new hosting.
- IHP PoC `amitaibu/ihp-eheza`: 61 commits, feature work stopped 2025-07-06, covers ≈3–5 %
  (5 authority + 3 general types, one table per type, no `deleted`, `base_revision` ignored,
  DB recreated instead of migrated), client branch `ihp-poc` changed the sync URLs.
  ⚠ `flake.nix`/`common.nix` there commit plaintext RDS password, session secret, AWS keys — rotate.
- Design gap worth fixing in a rewrite: any device token can pull any health-center shard.

**Position offered:** choose the storage shape before the language — a generic entity envelope
(uuid, type, global revision, shards[], person, encounter, deleted, JSONB data) keeps the 214
types as data, lets the cut-over keep `vid` values so devices need no re-pairing, and makes the
Drupal output the oracle for differential tests. Finalists presented: Django (admin covers the
staff UI nearly free, lowest bus-factor risk) vs IHP restarted on the envelope (type safety,
Gizra's Haskell investment). Phoenix = best runtime fit, worst team fit. See [[design-brief-backend-per-record-commit]].

**Acadia (acadia.engineering) assessed 2026-10-10 at the user's request:** Evan Czaplicki's
Elm-like database language + endpoint server (`acadia serve`, POST `/_endpoints`, generated Elm
client code). Public alpha 0.3.1 (2026-08-18), closed-source binary, Acadia Engineering ApS
(Denmark). Licences: Unregistered = evaluation/non-commercial, Personal = non-commercial
subscription (data-access-on-lapse clause); a Teams/commercial licence does NOT exist yet. SQLite
shipped, Postgres documented but not public; no List/array, Bytes, Date or JSON column types; no
jobs, no admin UI; Elm 0.19.3. Verdict given: not usable for E-Heza production today; revisit when
a commercial licence and Postgres ship. Related: [[elm-version-must-match-compiler-exactly]].

**How to apply:** when the user returns to this topic, start from the decision they made, not
from re-surveying; the inventory above is the baseline.
