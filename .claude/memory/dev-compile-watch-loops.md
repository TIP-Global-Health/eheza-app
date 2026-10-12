---
name: dev-compile-watch-loops
description: How the user runs live compile loops during development — so I don't kick off redundant one-shot compiles
metadata:
  node_type: memory
  type: user
  originSessionId: 77b80be7-a4e8-4a41-8f6c-9b16209262e0
---

- **Server-side Elm** (`server/elm/`): user runs `ddev elm:watch` during development — defined in `.ddev/commands/web/elm:watch`, invokes `server/elm/elm-watch.sh` which loops `elm make` on file changes.
- **Client-side Elm** (`client/src/elm/`): user runs `ddev gulp` (serves on localhost:3000, watches files) — already documented in CLAUDE.md.

How to apply: When working with the user on Elm changes, don't run a one-shot `elm make` "to confirm it compiles" if the user is likely already running the watch loop — they'll see errors first. Only run an explicit compile when (a) the user specifically asks, (b) you need the exit code programmatically (CI verification, pre-commit), or (c) the user has indicated the watcher is off. If in doubt, ask.
