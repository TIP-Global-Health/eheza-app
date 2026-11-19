---
name: no-blockquotes-in-answers
description: Never use Markdown blockquotes (>) for drafts or quoted text in answers — they render as bars and break listening
metadata:
  node_type: memory
  type: feedback
  originSessionId: 70339c46-5218-48fe-847b-5385229c0239
  modified: 2026-11-19T09:52:14.713Z
---

Do not wrap drafts (issue bodies, PR text, comments) or any quoted text in `>` blockquotes. Write them as normal paragraphs under a heading or between `---` rules.

**Why:** the user listens to answers via TTS (see global CLAUDE.md); the blockquote bars before every line interrupt hearing the section (user, 2026-11-19, on the B-595 issue draft: "Why do you add the | before every line? ... Write normally please.").

**How to apply:** for a draft, use a plain heading like "Draft issue" and write it as ordinary text; short verbatim quotes go inline in quotation marks.
