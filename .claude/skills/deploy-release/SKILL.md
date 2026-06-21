---
name: deploy-release
description: Guide and perform a FULL E-Heza production release for ONE site — build from `main`, deploy to Pantheon Dev, promote Dev → Test → Live, then cut the GitHub release + changelog. Trigger when the user wants to release, ship to production, go live, promote to Test/Live, or generate release notes for a site (rwanda / burundi / somalia). For deploying an arbitrary branch to a multidev/review env instead, use the `deploy-multidev` skill. Drives the safe steps itself and pauses for the interactive or production-affecting commands.
---

# Full Release (E-Heza → Pantheon, `main` → Live)

This skill performs a **full production release** for one **single site**: build from `main`, deploy to Pantheon **Dev**, promote **Dev → Test → Live**, then cut the **GitHub release + changelog**. It mirrors the runbook at https://github.com/Gizra/ihangane/wiki/Deployment and the commands in `server/RoboFile.php`.

> Just need to push a branch to a multidev/review env for testing? Use the **`deploy-multidev`** skill — this one is production-only and `main`-only.

**Shared reference** (site table, RoboFile internals, one-time Pantheon clone setup, troubleshooting): **`.claude/skills/_deploy-common/deploy-reference.md`** — read it before running.

## Execution model — who runs what

You (Claude) **drive every step except the deploy/promote commands** — those move code to Dev/Test/Live and need a human to review and confirm.

- 🟢 **You run** (via Bash on the host): git prep on `main`, all preflight checks, `ddev restart`, `ddev auth ssh`, `ddev gulp publish`, the terminus auth check/login, the post-deploy `fra`, plus tag math, `generate:release-notes`, and the GitHub release (`gh`). None of these are destructive, and on a normal ddev setup they're non-interactive (SSH keys in the agent, `TERMINUS_MACHINE_TOKEN` in the env). Only hand `ddev auth ssh` to the user if it prompts for a key passphrase.
- 🔴 **User runs — the deploy/promote commands only**: `ddev robo deploy:pantheon` (Dev) and `ddev robo deploy:pantheon-sync test|live`. **Offer each tee'd to a log** so you read the outcome yourself instead of asking for a paste:
  ```bash
  ddev robo deploy:pantheon 2>&1 | tee /tmp/deploy-dev.log
  ```
  `deploy:pantheon` has an interactive **"Commit changes and deploy?"** prompt (a human must review the change-set before confirming), and the Test→Live promotions are production, so a human triggers them. After each returns, `Read` its log.

**One site per run.** To release several sites, finish this flow, then re-invoke for the next site.

## Site → config mapping

Look up the chosen site's `EHEZA_SITE`, `PANTHEON_NAME`, and Pantheon clone dir (`server/.pantheon-<PANTHEON_NAME>`) in the **site table** in `.claude/skills/_deploy-common/deploy-reference.md`.

⚠️ Two distinct Burundi sites (`vhw`, `uvl`) share `EHEZA_SITE=burundi` but have different `PANTHEON_NAME`. Always pin the exact Pantheon site with the user — never assume "burundi".

---

## Step 0 — Gather inputs (ask the user)

This skill always runs the **full path** (`main` → Dev → Test → Live). Use **AskUserQuestion** to collect:

1. **Target site** — Rwanda / Burundi-vhw / Burundi-uvl / Somalia. Resolve to `EHEZA_SITE` + `PANTHEON_NAME` via the shared site table.
2. **Cut the GitHub release at the end?** (yes/no) — the tag + changelog + GitHub release, done once code is on Live.

Confirm the plan back in one line (site, `PANTHEON_NAME`, full release to Live, release-notes y/n) before proceeding.

## Step 1 — Preflight checks (🟢 you run; all must pass before deploying)

Run these and report a ✅/❌ checklist. **Stop and surface any ❌ — do not deploy past a failure.**

1. **Config matches the target site.** Read `.ddev/config.local.yaml` and confirm `EHEZA_SITE` and `PANTHEON_NAME` equal the chosen site's values.
   - If they don't match: tell the user to fix `.ddev/config.local.yaml` (the mandatory restart in Step 2 loads the new values). This is the #1 footgun — wrong `PANTHEON_NAME` pushes code to the **wrong Pantheon site**; wrong `EHEZA_SITE` builds the wrong site's data/config.
2. **`main` clean & level with `origin/main`.** Step 2 runs `git checkout main`, so you need not be sitting on `main` now — but it must be deploy-ready: in sync with the remote and free of *tracked* changes. The deploy aborts only on **tracked** changes; untracked files (e.g. `.ddev/`, `server/.pantheon-*`) are ignored, so check with `-uno` (not `-sb`, which would false-alarm on those):
   ```bash
   git fetch origin main --tags
   git rev-list --left-right --count main...origin/main   # expect "0	0" (level with origin/main)
   git status -s -uno                                     # expect empty (no tracked changes)
   ```
3. **All intended PRs merged into `main`.** Ask the user to confirm the release content is fully merged (you can show `git log --oneline origin/main -5` for context).
4. **Pantheon clone exists & clean.** Confirm `server/.pantheon-<PANTHEON_NAME>/` exists and is clean:
   ```bash
   git -C server/.pantheon-<PANTHEON_NAME> status -s -uno
   ```
   If the directory is missing, point the user to the one-time clone command in the shared reference — they must create it before deploying.
5. **Terminus authenticated.** `ddev auth ssh` covers the git *push* to Pantheon, but the post-deploy/promotion `terminus remote:drush` (`cc all` / `updb` / `uli`) and `fra` have separate auth — without it the deploy pushes code and *then* fails with *"You are not logged in"*, leaving the env on new code with a stale cache/registry:
   ```bash
   ddev exec terminus auth:whoami   # expect an email, not "You are not logged in"
   ```
   - If not logged in: set `TERMINUS_MACHINE_TOKEN` in `.ddev/config.local.yaml` (see shared reference) and `ddev restart`, or run `ddev exec terminus auth:login --machine-token=<token>`.

## Step 2 — Local prep

1. 🟢 You run: `git checkout main && git pull`.
2. **Restart DDEV — always, deciding reinstall first.** Ask the user (AskUserQuestion) whether to reinstall the project on this restart — **default No**:
   - **No (default):** ensure the `post-start` hook in `.ddev/config.local.yaml` is in its default state (only `- exec-host: ddev client-install` active; everything below commented). No reinstall.
   - **Yes:** uncomment the entire commented `post-start` block (see *Reinstall-on-restart toggle* in the shared reference) so the restart runs `site-install` + migrations + feature flags for the selected `$EHEZA_SITE`. Re-comment afterward if future restarts shouldn't reinstall.
   Then run `ddev restart` (local-only and safe to run; the reinstall answer is the confirmation for the destructive local-DB wipe a reinstall performs).
3. 🟢 You run: `ddev auth ssh` — injects SSH keys so the deploy can push to Pantheon. (Hand to the user only if it prompts for a key passphrase.)
4. 🟢 You run: `ddev gulp publish` — minified production build of the Elm client. Long (allow several minutes); let it finish before deploying.

## Step 3 — Deploy to Dev

🔴 User runs (the one human-review step) — offer it **tee'd to a log** so you read the outcome yourself:
```bash
ddev robo deploy:pantheon 2>&1 | tee /tmp/deploy-dev.log
```
Tell the user explicitly: **at the "Commit changes and deploy?" prompt, review the printed `git status` of the Pantheon repo** and confirm only if the change-set is exactly what they expect. When it returns, `Read` `/tmp/deploy-dev.log` to verify the push + auto-run `cc all`/`updb`/`uli` — don't ask for a paste.

What this command does automatically (so you don't double-run it): rsyncs the build into the Pantheon clone, commits + pushes to Pantheon `master`, then runs on **Dev** `cc all` (twice), `updb -y`, and `uli`. It does **NOT** run `fra`.

> Pantheon's `master` branch = the **Dev** environment, not production. This step only updates Dev; promotion to Test/Live is Step 5.

## Step 4 — Post-deploy (every env you touch)

🟢 You run — the one step the robo command skips:
```bash
ddev exec terminus remote:drush <PANTHEON_NAME>.<env> -- fra -y   # features revert all
```
Then sanity-check the env (open the `uli` login link from the deploy output, smoke-test the app). If the deploy added manual steps (new variables, migrations), run those too.

## Step 5 — Promote Dev → Test → Live

Do these **in order**, pausing for the user and re-verifying between each. Each `deploy:pantheon-sync` runs `terminus env:deploy` then `cc all`×2 + `updb -y` + `uli` on that env (but again **not** `fra`).

1. 🔴 User runs (tee'd): `ddev robo deploy:pantheon-sync test 2>&1 | tee /tmp/deploy-test.log` → `Read` the log → then you run Step 4 `fra` on `test` (🟢) → verify on the Test URL.
2. Pause for explicit user go-ahead (this next one hits production).
3. 🔴 User runs (tee'd): `ddev robo deploy:pantheon-sync live 2>&1 | tee /tmp/deploy-live.log` → `Read` the log → then you run Step 4 `fra` on `live` (🟢) → verify on the Live URL.

(Equivalent GUI path: Pantheon dashboard → Test tab → confirm deploy → Live tab → confirm deploy.)

## Step 6 — Cut the GitHub release (after code is on Live, if requested)

Run from the **eheza-app** repo (`origin` = `TIP-Global-Health/eheza-app`).

1. 🟢 Find the latest tag (this becomes the *previous* tag for the changelog):
   ```bash
   git fetch --tags origin
   git describe --tags --abbrev=0 origin/main
   ```
2. Decide the next tag **with the user**: patch bump (`v1.17.1 → v1.17.2`) for minor changes, minor bump (`→ v1.18.0`) for a new feature. Confirm before tagging.
3. 🟢 Create & push the new tag:
   ```bash
   git tag <new-tag> && git push origin <new-tag>
   ```
4. 🟢 Generate the changelog — **pass the PREVIOUS tag**, since `generate:release-notes` lists changes *since* the tag you give it:
   ```bash
   ddev robo generate:release-notes <previous-tag>
   ```
5. 🟢 Draft the release with `gh` (or hand the user the GitHub "new release" URL). Use the new tag, target `main`, title `Release <new-number>`. The body's first line states the deploy date and sites, e.g. `Deployed in production on <Month DD, YYYY> (<sites>)`, followed by the generator output starting at `## Changelog`:
   ```bash
   gh release create <new-tag> --target main --title "Release <new-number>" --notes "$(cat <<'EOF'
   Deployed in production on <Month DD, YYYY> (<sites>)

   <## Changelog ... output>
   EOF
   )"
   ```
   Confirm the date and site list with the user before publishing.

---

## Wrap-up

Report what was released: site, `PANTHEON_NAME`, each env reached (Dev/Test/Live), `fra` run per env, and the release tag/URL if cut. Note anything skipped or any preflight ❌ that blocked progress.

For deeper details (RoboFile internals, the one-time Pantheon clone setup, troubleshooting preflight failures), see `.claude/skills/_deploy-common/deploy-reference.md`.
