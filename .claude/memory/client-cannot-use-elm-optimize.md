---
name: client-cannot-use-elm-optimize
description: The Elm client cannot build with --optimize because Utils/AllDict.elm uses Debug.todo; publish runs gulp-uglify with mangle:false, so the shipped bundle is 9.4 MB; measured gains of mangle and --optimize
metadata: 
  node_type: memory
  type: project
  originSessionId: d3b65968-9dec-4acd-b66a-c9bd7185b7c6
---

`elm make --optimize` **fails outright** on `client/`: *"There are uses of the `Debug` module in the following modules: Utils.AllDict"*. `client/src/elm/Utils/AllDict.elm` (the vendored AllDict fork) has 7 `Debug.todo` calls — lines 182, 195, 448, 464, 482, 512, 659 — marking unreachable red-black-tree states.

Consequently `client/gulpfile.js` invokes `gulp-elm` with only `{debug: false, warn: false}` and **no `optimize` flag**. Even `gulp publish` ships un-`--optimize`d Elm output. Its only minification is `gulp-uglify` (uglify-js 3.4.10) with **`mangle: false`**, because of a DropZone mangling issue, and that setting applies to `Main.js` too.

**Why:** Elm's `--optimize` strips the record-field names and constructor boxing that `Debug.toString`/`Debug.todo` rely on, so the compiler refuses the combination.

**How to apply:**
- Do **not** assume `gulp publish` runs `--optimize`; it does not. The deployed `app/Main.js` is **9.4 MB raw / 1.10 MB gzip** (checked 2026-10-10 in `server/.pantheon-*/app/`). An earlier "3.4 MB terser" figure was a mangled build, not production.
- Measured 2026-10-10 with Elm 0.19.3 and uglify `-c`. Each row gives raw size / gzip / V8 parse time on a desktop:
  - today (dev-mode, no mangle): 9.39 MB / 1101 KB / 92 ms
  - mangle only: 3.46 MB / 789 KB / 48 ms
  - `--optimize` only: 7.85 MB / 956 KB / 63 ms
  - `--optimize` + mangle: 2.54 MB / 649 KB / 42 ms
  Most of the gain comes from mangling `Main.js`, which needs no Elm change.
- Replacing those 7 `Debug.todo` calls (e.g. return a sensible default, or restructure so the states are unrepresentable) would unlock Elm's own optimizations — record-field shortening and constructor unboxing — on top of terser. Worthwhile follow-up; not yet done.
- Because the app ships in dev mode, `Debug.todo` crash text is live in production, which is why [[elm-0192-debug-todo-region-offbyone]] is (mildly) relevant here.

Related: [[design-brief-assoclist-dict-migration]] (AssocList/AllDict is a vendored fork), [[elm-version-must-match-compiler-exactly]]
