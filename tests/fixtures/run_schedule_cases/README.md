The VM scenarios drive the generated coordinator with explicit service groups
and Oracle observations. Reversed control order remains observable; duplicate
and unknown ids do not repeat delivery. A premature readiness event keeps the
job, and stale readiness after completion or cancellation cannot land it again.

`pendingThenReady` pins the existing global per-branch Oracle coordinates:
begin is 0; each turn observes stop, wait, clock and then take. No live jobs,
clocks, waits or socket timing are needed by the verification cases.

The integration suite runs these cases with ordinary and hostile verification.
It separately exports the same coordinator and its laws, with sampled cases
removed, to Lean and requires every law to be universal. The resource-stub
scenarios are VM tests, not claims about the current proof lifter's job model.
