---
name: elm-0193-upgrade-assessment
description: "Elm 0.19.3 (2026-10-02), adopted via PR #2323 on 2026-10-11 — measured on our client — no language changes, same output except record field order, ~1/3 less compile RAM, ships our Debug.todo region fix"
metadata:
  node_type: memory
  type: project
  originSessionId: b09d0018-ac85-4b01-9197-55bb1331d51d
  modified: 2026-10-11T09:15:35.494Z
---

Elm 0.19.3 was released 2026-10-02. It has no language changes; the work is compiler internals (precise name types, Graph/MinimalCycle import-cycle errors, Bytes-based caches, ThreadSafe.Fork). Measured on `client/` on 2026-10-10. **Adopted: PR #2323** (issue #2322) merged 2026-10-11 as `1c1374062`, with mangling of `Main.js` and a `Main.js.map` for Rollbar. Host, DDEV and every pin are on 0.19.3 since then:

- **Output:** same line count. After normalising, the only differences are record-literal field ORDER (0.19.2 alphabetical, 0.19.3 by name length then name), renumbered `_vN$M` labels, the dev-mode warning URL, and the `Debug.todo` regions, now 1-based again. Only tests use `Debug.toString`, so the order change is inert for us.
- **Peak RSS** for a clean `elm make src/elm/Main.elm`: 0.19.2 ≈ 18.4 GB, 0.19.3 ≈ 12.2 GB. Wall time is noisy (37–52 s against 42–43 s), so there is no clear speed gain.
- Our upstream PR elm/compiler#2359 (off-by-one fix, see [[elm-0192-debug-todo-region-offbyone]]) ships in 0.19.3.
- npm is ready: `elm@latest-0.19.3` = `0.19.3-0`, `elm-test@0.19.3-0`.

**How to apply:** the lockstep list lives in [[elm-version-must-match-compiler-exactly]]. 0.19.3 still blocks `--optimize` on `Debug.todo` ([[client-cannot-use-elm-optimize]]).

**CI does not exercise the mangled bundle.** `install_client.sh` runs `gulp build` (unminified) unless `DEPLOY` is set, so e2e tests the dev bundle. The mangled `dist/` was checked by hand. On 2026-10-11 the full local Playwright suite ran against it, 65/65 passing. To do that, copy `Config.elm` over `Config.Deploy.elm` (or the build targets the prod backend), run `gulp publish`, and serve `dist/` with `python3 -m http.server 3000` in the container. Review round 1 added `Main.js.map` (5.1 MB, gulp-sourcemaps, `includeContent: false`) plus Rollbar `source_map_enabled`, so mangled traces still name Elm functions.

**DDEV verified 2026-10-10** by borrowing the main tree: the image rebuilds to 0.19.3 with `ddev stop && ddev start --skip-hooks` (DB kept), `ddev gulp` serves and runs the 0.19.3 bundle, and `gulp publish` in the container emits the mangled `dist/Main.js` (3.46 MB / 789 KB gz), which boots. The `ddev gulp publish` wrapper first `git pull`s `eheza-infra` over SSH, so it needs `ddev auth ssh` first.
