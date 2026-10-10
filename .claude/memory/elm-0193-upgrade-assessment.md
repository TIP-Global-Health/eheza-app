---
name: elm-0193-upgrade-assessment
description: "Elm 0.19.3 (2026-10-02) measured on our client — no language changes, same output except record field order, ~1/3 less compile RAM, ships our Debug.todo region fix"
metadata:
  node_type: memory
  type: project
  originSessionId: b09d0018-ac85-4b01-9197-55bb1331d51d
  modified: 2026-10-10T12:48:48.185Z
---

Elm 0.19.3 was released 2026-10-02. It has no language changes; the work is compiler internals (precise name types, Graph/MinimalCycle import-cycle errors, Bytes-based caches, ThreadSafe.Fork). Measured on `client/` on 2026-10-10. **PR #2323** (issue #2322) carries the upgrade plus mangling of `Main.js`:

- **Output:** same line count. After normalising, the only differences are record-literal field ORDER (0.19.2 alphabetical, 0.19.3 by name length then name), renumbered `_vN$M` labels, the dev-mode warning URL, and the `Debug.todo` regions, now 1-based again. Only tests use `Debug.toString`, so the order change is inert for us.
- **Peak RSS** for a clean `elm make src/elm/Main.elm`: 0.19.2 ≈ 18.4 GB, 0.19.3 ≈ 12.2 GB. Wall time is noisy (37–52 s against 42–43 s), so there is no clear speed gain.
- Our upstream PR elm/compiler#2359 (off-by-one fix, see [[elm-0192-debug-todo-region-offbyone]]) ships in 0.19.3.
- npm is ready: `elm@latest-0.19.3` = `0.19.3-0`, `elm-test@0.19.3-0`.

**How to apply:** the bump is a lockstep cut, per [[elm-version-must-match-compiler-exactly]]. That means 4 `elm.json` files, `ci-scripts/install_client.sh`, `test_elm.sh`, `install_elm_review.sh`, `.ddev/web-build/Dockerfile`, `.gitpod.yml`, `client/package.json` and the host-global elm. 0.19.3 still blocks `--optimize` on `Debug.todo` ([[client-cannot-use-elm-optimize]]).

**CI does not exercise the mangled bundle.** `install_client.sh` runs `gulp build` (unminified) unless `DEPLOY` is set, so e2e tests the dev bundle. The mangled `dist/` was checked by hand: it boots to Device Status with no console errors. Until #2323 merges, verify with a scratchpad 0.19.3 `elm` and `elm-test@0.19.3-0`; the host-global stays 0.19.2 for develop.
