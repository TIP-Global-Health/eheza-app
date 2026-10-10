---
name: mandatory-form-changes-meaning-of-no-answer
description: ⛔ making a form/task compulsory changes what every stored shape of it means — re-ask before shipping the two halves together
metadata:
  type: feedback
---

⛔ **When a change makes a form or task compulsory, re-ask what every stored shape of that
form now means.** A "this record says nothing" branch that was rare while the form was
optional becomes the common case once it is required, and can silently outrank fresh
clinical answers.

**Why:** B-289 (PR #2222) shipped two halves at once — the NCD pregnancy test became
expected at *every* encounter, and pregnancy status started reading the most recent
encounter that "answered". The second half treated a not-performed test as no answer.
But `laboratoryTaskCompleted` requires the measurement to exist once the task is expected,
so every completed encounter now stores one — and 707 of 749 live records are
`not-indicated`. The skip branch went from rare to routine and inherited stale positives
for up to 9 months. Review round 2 caught it; round 1 had not.

**How to apply:** before shipping, enumerate the stored shapes (here: each
`TestExecutionNote`) and ask what each one *proves* rather than what it lacks. The proof
is often in the FORM, not the value — a why-not note existed only because the nurse had
already answered "not known to be pregnant", yet `knownAsPositive` is never stored. Read
the view that produces the value, not just its type. See [[verify-by-running-not-reasoning]]
and [[read-the-issue-for-requirements]].
