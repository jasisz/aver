The VM scenarios drive the generated loop with explicit service groups and
explicit seating. Reversed control order remains observable; duplicate and
unknown ids do not repeat delivery. A keyed family is seated once per key,
a key that leaves the list drops its instance, and a key whose instance
returned is not seated again while it stays listed.

The laws in `main.av` state the loop's own invariants over the generated
functions, which a program may call by name: a late answer changes nothing
and is counted, an Err keeps the request's instance, an Ok raises it, a
deadline gates the ask and never makes the turn wait longer than it asked for,
and a Settled wake opens only once its module has answered something other
than Settled.

The integration suite runs these cases with ordinary and hostile
verification. It separately exports the same loop and its laws to Lean and
requires every law to be universal.
