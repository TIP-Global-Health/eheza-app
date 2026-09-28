---
name: client-cannot-use-elm-optimize
description: The Elm client cannot build with --optimize because Utils/AllDict.elm uses Debug.todo; gulp-elm runs unoptimized and terser does all minification
metadata: 
  node_type: memory
  type: project
  originSessionId: d3b65968-9dec-4acd-b66a-c9bd7185b7c6
---

`elm make --optimize` **fails outright** on `client/`: *"There are uses of the `Debug` module in the following modules: Utils.AllDict"*. `client/src/elm/Utils/AllDict.elm` (the vendored AllDict fork) has 7 `Debug.todo` calls — lines 182, 195, 448, 464, 482, 512, 659 — marking unreachable red-black-tree states.

Consequently `client/gulpfile.js` invokes `gulp-elm` with only `{debug: false, warn: false}` and **no `optimize` flag**. Even `gulp publish` ships un-`--optimize`d Elm output; all size reduction comes from terser + gzip afterwards.

**Why:** Elm's `--optimize` strips the record-field names and constructor boxing that `Debug.toString`/`Debug.todo` rely on, so the compiler refuses the combination.

**How to apply:**
- Do **not** assume `gulp publish` runs `--optimize`; it does not. Any benchmark of "the production build" is really a dev-mode build (measured 2026-07-08: dev bundle 12.1 MB raw → 3.4 MB terser → 778 KB gzip).
- Replacing those 7 `Debug.todo` calls (e.g. return a sensible default, or restructure so the states are unrepresentable) would unlock Elm's own optimizations — record-field shortening and constructor unboxing — on top of terser. Worthwhile follow-up; not yet done.
- Because the app ships in dev mode, `Debug.todo` crash text is live in production, which is why [[elm-0192-debug-todo-region-offbyone]] is (mildly) relevant here.

Related: [[design-brief-assoclist-dict-migration]] (AssocList/AllDict is a vendored fork), [[elm-version-must-match-compiler-exactly]]
