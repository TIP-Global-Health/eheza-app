---
name: deploy-multidev
description: Deploy an arbitrary branch to a Pantheon multidev (review/feature) environment for ONE E-Heza site — build from the selected branch and push it to a named multidev env. Trigger when the user wants to deploy a branch to a multidev/review/feature/test env, spin up or update a review environment, or push a non-`main` branch to Pantheon for testing. NOT for production — use the `deploy-release` skill for the full `main` → Live release. Drives the safe steps itself and pauses for the interactive or production-affecting commands.
---

# Deploy to Multidev (E-Heza → Pantheon)

This skill deploys a chosen **git branch** to a Pantheon **multidev** (review/feature) environment for one **single site**. It's the low-risk path: it only updates an isolated multidev env — never Dev/Test/Live, and it never cuts a release.

> Releasing to production (`main` → Dev → Test → Live + GitHub release)? Use the **`deploy-release`** skill instead.

**Shared reference** (site table, RoboFile internals, one-time Pantheon clone setup, troubleshooting): **`.claude/skills/_deploy-common/deploy-reference.md`** — read it before running.

## Execution model — who runs what

You (Claude) **drive the safe steps** and **pause at the risky ones**. Never run a step marked 🔴 yourself.

- 🟢 **You run** (via Bash on the host): git prep (checkout/pull the source branch) and all read-only preflight checks.
- 🔴 **User runs** (give them the exact command; they run it with `! <command>` so output lands here, then you continue): `ddev auth ssh`, `ddev gulp publish`, `ddev robo deploy:pantheon <env>`, and post-deploy `fra`.

Pausing matters because `ddev robo deploy:pantheon <env>` shows a `git status` of the Pantheon repo and an interactive **"Commit changes and deploy?"** prompt — a human must review that change-set before confirming. `ddev auth ssh` is also interactive.

**One site per run.**

## Site → config mapping

Look up the chosen site's `EHEZA_SITE`, `PANTHEON_NAME`, and Pantheon clone dir (`server/.pantheon-<PANTHEON_NAME>`) in the **site table** in `.claude/skills/_deploy-common/deploy-reference.md`.

⚠️ Two distinct Burundi sites (`vhw`, `uvl`) share `EHEZA_SITE=burundi` but have different `PANTHEON_NAME`. Always pin the exact Pantheon site with the user — never assume "burundi".

---

## Step 0 — Gather inputs (ask the user)

Use **AskUserQuestion** (and free text where needed) to collect:

1. **Target site** — Rwanda / Burundi-vhw / Burundi-uvl / Somalia. Resolve to `EHEZA_SITE` + `PANTHEON_NAME` via the shared site table.
2. **Source branch to deploy** — any git branch (default: the current branch). This is what gets *built*; it is independent of the multidev env name.
3. **Target multidev env name** — the existing Pantheon multidev environment to deploy into (e.g. `review-x`). The build is pushed to the Pantheon branch of the same name.

Confirm the plan back in one line (site, `PANTHEON_NAME`, source branch → multidev `<env>`) before proceeding.

## Step 1 — Preflight checks (🟢 you run; all must pass)

Report a ✅/❌ checklist. **Stop and surface any ❌.**

1. **Config matches the target site.** Read `.ddev/config.local.yaml`; confirm `EHEZA_SITE` and `PANTHEON_NAME` equal the chosen site's values.
   - If not: tell the user to fix `.ddev/config.local.yaml` **and then `ddev restart`** (web_environment vars only reload on restart). Wrong `PANTHEON_NAME` pushes to the **wrong Pantheon site**.
2. **Source branch exists & working tree clean.** The branch is real, and the eheza-app working tree has no *tracked* changes (the deploy aborts on tracked changes; untracked files like `.ddev/` are ignored):
   ```bash
   git rev-parse --verify <source-branch>   # exists locally...
   git ls-remote --heads origin <source-branch>   # ...or on origin
   git status -s -uno                        # expect empty (no tracked changes)
   ```
3. **Pantheon clone exists, clean, and has the target multidev branch.** The deploy runs `git checkout <env>` inside the clone and aborts with *"Specified branch `<env>` does not exist"* if that branch isn't there:
   ```bash
   git -C server/.pantheon-<PANTHEON_NAME> status -s -uno              # clean
   git -C server/.pantheon-<PANTHEON_NAME> fetch origin                # refresh
   git -C server/.pantheon-<PANTHEON_NAME> rev-parse --verify <env> 2>/dev/null \
     || git -C server/.pantheon-<PANTHEON_NAME> rev-parse --verify origin/<env>
   ```
   - If the multidev branch is missing: the env must exist on Pantheon first. Tell the user to create the multidev environment in the Pantheon dashboard (or via terminus), then in the clone `git fetch origin && git checkout <env>`. Without it, the deploy can't proceed.

## Step 2 — Local prep

- 🟢 You run: `git checkout <source-branch> && git pull` (pull only if it tracks a remote).
- 🔴 User runs (paused): `ddev auth ssh` — authenticates to Pantheon over SSH (interactive).
- 🔴 User runs (paused): `ddev gulp publish` — minified production build of the Elm client from the checked-out branch. Long; let it finish.

## Step 3 — Deploy to the multidev env

🔴 User runs (paused): `ddev robo deploy:pantheon <env>`

Tell the user explicitly: **at the "Commit changes and deploy?" prompt, review the printed `git status` of the Pantheon repo** and confirm only if the change-set is exactly what they expect.

What this does automatically (so you don't double-run it): rsyncs the build into the clone, checks out the `<env>` branch, commits + pushes to it, then runs on that **multidev** env `cc all` (twice), `updb -y`, and `uli`. It does **NOT** run `fra`, and it touches **only** that multidev env.

## Step 4 — Post-deploy

🔴 User runs (paused) — the one step the robo command skips:
```bash
ddev exec terminus remote:drush <PANTHEON_NAME>.<env> -- fra -y   # features revert all
```
Then verify: open the `uli` login link from the deploy output (or the multidev URL `https://<env>-<PANTHEON_NAME>.pantheonsite.io`) and smoke-test. Run any manual steps the change introduced.

---

## Wrap-up

Report: site, `PANTHEON_NAME`, source branch, multidev `<env>` deployed to, and that `fra` was run. No Test/Live promotion and no release happen here — for that, switch to `deploy-release`. Note any preflight ❌ that blocked progress.
