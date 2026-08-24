---
name: elm-0192-debug-todo-region-offbyone
description: "Elm 0.19.2 emits 0-based source regions in --report=json and Debug.todo, so both are off by one; root cause and fix identified"
metadata: 
  node_type: memory
  type: reference
  originSessionId: d3b65968-9dec-4acd-b66a-c9bd7185b7c6
---

Elm **0.19.2** emits **0-based** source regions where 0.19.1 emitted 1-based ones. Verified 2026-07-08. It is *not* limited to `Debug.todo`.

**Root cause.** 0.19.2 repacked `A.Position` into a `Word64#` for memory density. `compiler/src/Reporting/Annotation.hs` says so itself: *"The initial value is (0,0) so conversion is needed to get `editor coordinates` which start at (1,1)"*. `Reporting.Render.Code` converts (`show (row + 1)`), so the **terminal error report is correct**. The only two sites that emit machine-readable coordinates call `A.toRowCol` raw and never convert:

- `Reporting.Error.encodeRegion` -> `elm make --report=json` (editors, `elm-language-server`)
- `Generate.JavaScript.Expression.regionToJsExpr` -> `Debug.todo` / `Debug.todoCase` regions baked into the bundle

**Symptoms.** A type error on line 6 col 5 is reported by `--report=json` as line 5 col 4. A `Debug.todo` on line 17 crashes with ``TODO in module `Main` on line 16``. Every region differs from 0.19.1 by exactly `(-1, -1)`.

**Fix** (build-verified; needs GHC >= 9.8 for `ExtendedLiterals`, so build it in a `haskell:9.8.4` container rather than on the host, which has 9.6.7): add a `toEditorRowCol` helper (`toRow`/`toCol` + 1) to `Reporting/Annotation.hs`, export it, and route both emit sites through it. After that change `toRowCol` has zero remaining callers, so redefining `toRowCol` itself to be 1-based is the minimal patch. A uniform +1 on row and column reproduces 0.19.1 byte-for-byte. **Reported and fixed upstream: elm/compiler#2358 (issue), elm/compiler#2359 (PR from anvmn/compiler:fix-0-based-source-regions).** The patch was build-verified with GHC 9.8.4 in a `haskell:9.8.4` container (`cabal build exe:elm`, so under the package's `-Wall -Werror`) and the resulting compiler reproduces 0.19.1 exactly on both symptoms; the human-readable report stays correct (not double-incremented).

**How to apply:** when chasing a `TODO in module ... on line N` crash from a 0.19.2 build, look at line **N+1**. Same for any editor squiggle driven by `--report=json`. Relevant here because the client ships in dev mode (see [[client-cannot-use-elm-optimize]]) with 7 `Debug.todo` calls in `Utils/AllDict.elm`.

Other 0.19.2 codegen deltas, all semantically inert (checked by diffing emitted `Main.js`, identical line count):
- string-literal escaping style changed: 0.19.2 emits six-character `\uXXXX` escapes (for newline, single quote, double quote, backslash) where 0.19.1 used short escapes, and conversely uses short `\r` / `\f` where 0.19.1 used the `\uXXXX` forms. Raw bundle +10 KB, but -8 bytes after terser and +21 bytes gzipped;
- order of independent `var x = _v0.field;` record reads is reversed.

**Watch out:** `elm make --optimize` prints `Success! Compiled N modules.` *before* running the Debug check and failing. Grepping for `Success!` through a pipe swallows the exit code and makes a failed build look green. See [[client-cannot-use-elm-optimize]].

Related: [[elm-version-must-match-compiler-exactly]], [[gizra-elm-form-fork]]
