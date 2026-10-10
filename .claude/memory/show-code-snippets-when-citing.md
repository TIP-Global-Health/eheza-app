---
name: show-code-snippets-when-citing
description: "⛔ HARD RULE (user, 2026-09-01) — mentioning code in a file means SHOWING the snippet, never a bare file:line reference"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: e12b063c-8ef4-450a-973d-07611a939f8a
  modified: 2026-09-01T14:06:58.131Z
---

Whenever an answer mentions code in a file — presenting a backlog item, analysing a review
finding, explaining a mechanism — it must show the code snippet itself, in this shape:

```elm
-- Backend/Update.elm:7854-7878
StockUpdateRevision uuid data ->
    let
        modelWithMappedStockManagement =
            mapStockManagementMeasurements          -- HC cache only
                healthCenterId
                (\measurements -> { measurements | stockUpdate = ... })
                modelWithStockUpdateRecalc
        ...
```

A `-- path:lines` comment header, then the real code — elided with `...` where irrelevant,
with short inline annotations where they help (`-- HC cache only`).

**Why:** the user asked for it as a hard rule (2026-09-01, citing the B-299 presentation as
the example to follow). A bare `file.elm:123` reference forces the reader to open the file
to follow the argument; the snippet makes the claim checkable in place.

**How to apply:** every item presentation, review-finding analysis, and mechanism
explanation that cites a location shows the lines it is talking about. Re-read the code
first — the snippet must be today's code, not a paraphrase from the entry. Related:
[[pr-first-review-workflow]].
