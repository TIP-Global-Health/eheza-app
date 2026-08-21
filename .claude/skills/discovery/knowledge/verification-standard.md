# What "verified" means here

A finding enters the backlog only when the orchestrating session has traced it. An agent's
report is a candidate; the trace is the evidence. The cost of getting this wrong is not a wasted
round — it is a wrong premise that survives into an implementation session and gets built.

Check all five before recording:

**1. The code path runs.** Find the caller, and the caller's caller, until you reach something a
user or a cron actually triggers. Dead branches and unreferenced helpers have been "found" more
than once.

**2. The value is really used.** A wrong value that nothing reads is not a defect. Follow it to
where it is displayed, stored, or decided on.

**3. Every layer it travels through.** A payload can satisfy the Elm decoder and still fatal the
PHP that receives it. Check each hop, not the first.

**4. The runtime and production values.** What the flag is set to, what the data actually
contains, which site it applies to. `EHEZA_SITE` splits behaviour; several "bugs" have been
site-correct.

⛔ **A symptom seen in live data must be diagnosed against the DEPLOYED code**, which can be
hundreds of commits behind `develop`. The Pantheon checkouts under `server/.pantheon-*` are what
runs — but their working trees can themselves be months stale. Read via
`git show origin/master:<path>` and check `git log -1 origin/master` first. Reading the stale
working tree has inverted a conclusion.

**5. It is not already known.** Grep the backlog for the **symptom** and the **function name**,
not just item ids. Check the coverage map's declined classes and cleared rows.

## Refuting is a result

An item that dies under verification is worth recording as refuted, with the reason. It stops the
next round re-finding it, and the reasoning is usually reusable. The same goes for a candidate
that turns out to be already fixed.

## Adversarial pass

For anything subtle, try to kill it before recording it: state the claim, then look for the
reading of the code that makes it false. Prefer the refutation you can prove by running something
— a script in the scratchpad against the real function beats an argument about what the code
means.
