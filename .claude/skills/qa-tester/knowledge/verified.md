# QA Verification Ledger — E-Heza

One line per behavior manually verified. These are one-time verifications: before planning a
run, check whether the behavior is already here. Append at the end of every run.
Evidence is the recording's path relative to the repo root
(`client/qa-recordings/<pr>/qa-<pr>-<scenario>.gif`).
Result is PASS or FAIL; a FAIL row links the PR comment or issue that reports the bug, and
the behavior is re-verified (new row) after the fix — only a PASS row counts as "verified once".

| Date | Issue | PR | What was verified | Result | Evidence |
|------|-------|----|-------------------|--------|----------|
| 2026-08-20 | #2123 | #2124 | Prenatal Laboratory (nurse, point-of-care): blood glucose 12 refused with unit warning popup, save blocked | PASS | client/qa-recordings/2124/qa-2124-prenatal-lab-point-of-care.gif |
| 2026-08-20 | #2123 | #2124 | Same form: 1250 (above range) refused; 120 saves and completes the task | PASS | client/qa-recordings/2124/qa-2124-prenatal-lab-point-of-care.gif |
| 2026-08-20 | #2123 | #2124 | Labs-history form (subsequent encounter → Laboratory → History → Update on pending Blood Sugar): 12.5 refused with popup; 120 saves and the entry leaves the pending list | PASS | client/qa-recordings/2124/qa-2124-prenatal-labs-history.gif |
| 2026-08-20 | #2123 | #2124 | NCD Laboratory (nurse, point-of-care): 12 refused with popup; 120 saves | PASS | client/qa-recordings/2124/qa-2124-ncd-lab-point-of-care.gif |
| 2026-08-20 | #2123 | #2124 | NCD recurrent Lab Results (Case Management): 12 refused with popup; 120 saves | PASS | client/qa-recordings/2124/qa-2124-ncd-recurrent-lab-results.gif |
| 2026-08-20 | #2123 | #2124 | Lab tech (PIN 3333) ANC Lab Results via Case Management: 12 refused with popup; 120 saves | PASS | client/qa-recordings/2124/qa-2124-labtech-lab-results.gif |
| 2026-08-20 | #2123 | #2124 | Backend stores the corrected value: prenatal (nurse-ordered→labs-history), prenatal (lab tech) and NCD random blood sugar nodes all hold sugar_count=120 after sync, never the refused reading | PASS | drush query, nids 533/567/582 |
