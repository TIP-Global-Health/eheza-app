---
name: release-issue-reconciliation
description: "Recipe for closing delivered-but-still-open issues after a release (PRs merge to develop, non-default, so Fixes"
metadata: 
  node_type: memory
  type: project
  originSessionId: 90b8cb68-cb52-4552-a1b0-2c0ef85b1ec0
---

**Why this exists:** eheza-app PRs merge into `develop`, which is NOT the repo's default branch (`main`). GitHub only auto-closes `Fixes/Closes/Resolves #N` issues on a merge into the DEFAULT branch — so here the issues stay OPEN through develop merges and linger until a release (develop→main) delivers them. After each release, a manual sweep is needed to close the delivered issues. Validated 2026-07-07 on the v1.18.0 release (closed 39 delivered-but-open issues: 28 from the v1.18.0 window + 11 in the 1679–1761 range; correctly left 4 open).

**Recipe (per release):**
1. Window: `gh release list` → new tag + prior tag and their dates.
2. PRs in the release: `git fetch --tags`; `git log <prevtag>..<newtag> --merges --oneline | grep -oE "Merge pull request #[0-9]+" | grep -oE "[0-9]+" | sort -u`.
3. Each PR's referenced issue: this host's `gh` LACKS the `closingIssuesReferences` JSON field, so fetch `gh pr view N --json number,title,body` and regex title+body for `(?i)\b(close[sd]?|fix(e[sd])?|resolve[sd]?|issues?)\s*:?\s+#(\d+)`. ALSO check branch names for PRs with NO keyword ref — `issue-NNNN` or `NNNN-slug` implies issue NNNN (e.g. #1841 branch `issue-1823` → issue #1823; the ~5 cleanup PRs #1754–1762 referenced their issue #1753–1761 only by matching title, not keyword).
4. Resolve each referenced number: `gh api repos/TIP-Global-Health/eheza-app/issues/N` returns PRs too (shared numbering) — exclude where `.pull_request` is set; keep `.state=="open"` issues.
5. Close: `gh issue close N --comment "Delivered in {release} via #PR"`. (`gh issue close --comment` prints `✓ Closed issue #N` to STDERR — that is success, not an error.)

**Judgment caveats (do NOT blind-close):**
- A PR body may LIST several issues but deliver only some. Real 2026-07-07 catch: PR #1731 said "Issue: #1732, #1733" but only shipped the FBF Distribution report (#1732) — the stock-management report type (#1733) was never built → left #1733 OPEN. Verify the PR actually delivers each named issue.
- `[META]`/umbrella tracker issues (e.g. #1686 Playwright E2E) stay open by design.
- Issues with 0 merged-PR references are genuinely unresolved (e.g. #1682, #1697) → leave open.
- Release attribution: infer from merge-date vs tag dates; for a boundary case use `git tag --contains <pr.mergeCommit.oid>` and take the first `v1.*` tag (authoritative — used to confirm #1719→v1.17.0 on the tag-cut day).

Related: [[gh-pr-edit-projectcards-workaround]] (this repo's Projects-classic quirks), [[delete-branch-on-merge]], deploy-release skill.
