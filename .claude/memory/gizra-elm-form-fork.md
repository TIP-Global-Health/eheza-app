---
name: gizra-elm-form-fork
description: Gizra now owns/publishes Gizra/elm-form (fork of archived etaque/elm-form); Elm publishing rules + why the fork exists
metadata: 
  node_type: memory
  type: project
  originSessionId: d3b65968-9dec-4acd-b66a-c9bd7185b7c6
---

As of 2026-07-08 the client depends on **`Gizra/elm-form` 1.0.0** (repo `Gizra/elm-form`, published to package.elm-lang.org), not `etaque/elm-form`. Gizra therefore *maintains an Elm package*: any future elm-form change must be committed, tagged, and `elm publish`ed from that repo — there is no other release path.

**Why it exists:** `etaque/elm-form` 4.0.0 (latest; GitHub-archived since 2021) declares `elm-explorations/test` as a **runtime** dependency pinned `1.0.0 <= v < 2.0.0`, because it ships `Form.Test` helpers as public API. That transitively forbids `elm-explorations/test` 2.x, which `elm-test` >= 0.19.1-revision7 (and every 0.19.2-line release) requires. The fork moves `Form.Test*` from `src/` into `tests/`; the six exposed modules are byte-identical to upstream 4.0.0.

**Why:** without the fork, the Elm 0.19.2 compiler upgrade is impossible — `elm make` fails with `INCOMPATIBLE DEPENDENCIES`.

**How to apply:**
- Elm packages are **immutable**: a published version can never be unpublished or altered. Get it right before `elm publish`.
- A **new package name must publish as 1.0.0** ("All packages start with initial version"), even when forking a 4.0.0 package.
- Elm fetches packages from **GitHub zipballs** (integrity-checked by a hash in `endpoint.json`). Content is tamper-evident, but *availability depends on the repo surviving* — two sibling elm-form forks (`greg-enbala`, `jwheeler-cp`) are published yet 404 on install. CI caches nothing under `~/.elm`, so a deleted repo breaks CI. That is why we self-host rather than depend on `enbala/elm-form`.
- Do **not** switch to `scrive/elm-form` or `kutyel/elm-form`: their `Form.elm`, `Form/Init.elm`, `Form/Input.elm`, `Form/Validate.elm` diverge from upstream (behaviour changes).
- If someone ever needs `Form.Test` / `Form.Test.ValidationExpectation`, they are in the fork's `tests/`, not the published API.

Related: [[elm-fulltest-needs-elm-make-main]], [[elm-version-must-match-compiler-exactly]]
