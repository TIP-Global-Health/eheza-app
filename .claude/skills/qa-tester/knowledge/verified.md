# QA Verification Ledger — E-Heza

One line per behavior manually verified. These are one-time verifications: before planning a
run, check whether the behavior is already here. Append at the end of every run.
Evidence is the recording's path relative to the repo root
(`client/qa-recordings/<pr>/qa-<pr>-<scenario>.mp4`).
Result is PASS or FAIL; a FAIL row links the PR comment or issue that reports the bug, and
the behavior is re-verified (new row) after the fix — only a PASS row counts as "verified once".

| Date | Issue | PR | What was verified | Result | Evidence |
|------|-------|----|-------------------|--------|----------|
| 2026-08-20 | #2123 | #2124 | Prenatal Laboratory (nurse, point-of-care): blood glucose 12 refused with unit warning popup, save blocked | PASS | client/qa-recordings/2124/qa-2124-prenatal-lab-point-of-care.mp4 |
| 2026-08-20 | #2123 | #2124 | Same form: 1250 (above range) refused; 120 saves and completes the task | PASS | client/qa-recordings/2124/qa-2124-prenatal-lab-point-of-care.mp4 |
| 2026-08-20 | #2123 | #2124 | Labs-history form (subsequent encounter → Laboratory → History → Update on pending Blood Sugar): 12.5 refused with popup; 120 saves and the entry leaves the pending list | PASS | client/qa-recordings/2124/qa-2124-prenatal-labs-history.mp4 |
| 2026-08-20 | #2123 | #2124 | NCD Laboratory (nurse, point-of-care): 12 refused with popup; 120 saves | PASS | client/qa-recordings/2124/qa-2124-ncd-lab-point-of-care.mp4 |
| 2026-08-20 | #2123 | #2124 | NCD recurrent Lab Results (Case Management): 12 refused with popup; 120 saves | PASS | client/qa-recordings/2124/qa-2124-ncd-recurrent-lab-results.mp4 |
| 2026-08-20 | #2123 | #2124 | Lab tech (PIN 3333) ANC Lab Results via Case Management: 12 refused with popup; 120 saves | PASS | client/qa-recordings/2124/qa-2124-labtech-lab-results.mp4 |
| 2026-08-20 | #2123 | #2124 | Backend stores the corrected value: prenatal (nurse-ordered→labs-history), prenatal (lab tech) and NCD random blood sugar nodes all hold sugar_count=120 after sync, never the refused reading | PASS | drush query, nids 533/567/582 |
| 2026-08-21 | #1998 (epic #2006) | #2007 | Well Child Nutrition Assessment (nurse): height 1050, MUAC 120, weight 8500 each refused with a warning naming the measurement, its unit and the range, matching the sentence above the input; Save stays active (the task count answers), form kept, nothing saved; corrected weight 14 saves and completes the task | PASS | client/qa-recordings/2007/qa-2007-wellchild-nutrition-assessment.mp4 |
| 2026-08-21 | #2003 (epic #2006) | #2007 | ANC Examination → Nutrition Assessment: height 1600 + weight 850 + MUAC 125 entered together are refused by ONE warning naming all three, in the order the form asks for them, each with its own unit and range | PASS | client/qa-recordings/2007/qa-2007-anc-examination.mp4 |
| 2026-08-21 | #2003 (epic #2006) | #2007 | ANC Examination → Obstetrical Exam: fundal height 120 refused with "recorded in centimetres, allowed values between 1 and 60"; the other task's still-wrong values are not named (the check is per task, as the save is) | PASS | client/qa-recordings/2007/qa-2007-anc-examination.mp4 |
| 2026-08-21 | — | #2007 | Observation, not a PR defect: a warning is only forgotten by its own Close button, so leaving by browser Back (the only route the overlay does not block) leaves it standing on return. Reproduced on Well Child and on ANC, where it survived navigating to a group session and back. PR #2007 closed this hole and then reverted the fix (commit 50501d2b73 "Leave the warning where it is") as out of scope | NOTE | client/qa-recordings/2007/qa-2007-stale-warning-after-navigation.mp4 |
| 2026-08-21 | #1998/#2009 (epic #2006) | #2007 | Well Child **newborn exam** (CHW, Birth History): Apgar 36 + Apgar 30000 + birth weight 3 + birth length 0.5 entered together are refused by one warning naming all four in the order the form asks for them — Apgar 0–10 ×2, birth weight 300–7000 g with "a weight of 3 kilograms is entered as 3000", birth length 15–60 cm; corrected 8/9/3000/50 saves and the activity moves to Completed | PASS | client/qa-recordings/2007/qa-2007-newborn-exam.mp4 |
| 2026-08-21 | #1981 (epic #2006) | #2007 | Child Scorecard NCDA (CHW), step 1 Antenatal Care & Newborn: birth weight 3 refused with the grams warning and the **step does not advance**; 3200 goes on | PASS | client/qa-recordings/2007/qa-2007-scorecard-birth-weight.mp4 |
| 2026-08-21 | #2005 (epic #2006) | #2007 | Child Scorecard NCDA (CHW), step 4 Nutrition Assessment: weight 8500 + MUAC 120 refused by one warning naming both, and NOT naming the birth weight from step 1 that is now in range; 8.5 / 12.0 goes on | PASS | client/qa-recordings/2007/qa-2007-scorecard-weight-muac.mp4 |
| 2026-08-21 | #2002 (epic #2006) | #2007 | Group session (CHW, own group): child height 1050 refused, 85 saves; the app then moves to the MUAC activity and that form carries **no** stale height warning (the clear on leaving an activity, Pages/Participant/Update.elm); MUAC 125 refused naming only MUAC, 12 saves | PASS | client/qa-recordings/2007/qa-2007-group-session.mp4 |
