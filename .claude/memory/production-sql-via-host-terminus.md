---
name: production-sql-via-host-terminus
description: "How to run read-only SQL on a Pantheon live DB without tripping the auto-mode classifier — host terminus, one plain command matching the allow rule"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 9271c911-fce5-4489-8008-4c3d17619bcf
  modified: 2026-09-11T11:15:15.258Z
---

Run production read-only SQL as a single plain command on the HOST, exactly in the shape the
allow rule expects:

    terminus remote:drush ihangane.live -- sql-query "SELECT ..."

(sites: `ihangane` Rwanda, `vhw` / `uvl` Burundi, `tip-somalia` Somalia; `sqlq` also allowed).

**Why:** `.claude/settings.local.json` allows `Bash(terminus remote:drush <site>.live -- sql-query *)`
as a command PREFIX. A compound command (`Q="..."; ddev exec terminus ... | grep | tail`) matches no
rule, falls to the auto-mode classifier, and is blocked as a production DB access — this happened
2026-09-11 and the user asked why it "constantly fails". `ddev exec terminus` inside the container
is not what the rules name, and it needs `ddev auth ssh` + `ddev terminus-auth` on top; host
`terminus` at `~/.local/bin/terminus` is already logged in and has the SSH key.

**How to apply:** one query per Bash call, no variable assignments before the command, no pipes
after it (the `[notice]` trailer is harmless). Put the SQL inline in double quotes. Related:
[[deploy-tooling-and-aos-backend-state]].
