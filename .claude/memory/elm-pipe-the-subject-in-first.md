---
name: elm-pipe-the-subject-in-first
description: "Elm style — when a call's last argument is a computed expression (e.g. a List.filter), start with that expression and pipe it into the call, never leave it trailing after a long lambda"
metadata:
  node_type: memory
  type: feedback
  originSessionId: ffbbba47-6897-4f8a-a3af-875ce8f2c04b
  modified: 2026-11-23T09:21:51.684Z
---

When a function's last argument is itself an expression, write that expression first and `|>` it into the call:

    List.filter (\p -> Set.member p.id visited) scopeRecords
        |> List.filterMap
            (\p -> ...)

not `List.filterMap (\p -> ...long lambda...) (List.filter ... scopeRecords)`.

**Why:** the user flagged it on B-628 (2026-11-23, `generatePostnatalCareReportData`): a trailing argument after a 50-line lambda hides what the list is.

**How to apply:** whenever the data argument is anything more than a plain name, lead with it and pipe. See also [[code-duplication-rule]].
