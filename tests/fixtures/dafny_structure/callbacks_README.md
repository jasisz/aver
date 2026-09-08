# Checked named callbacks

`callbacks_positive.av` proves one and two applications of a named increment
function. `callbacks_imported/main.av` passes a callback whose record parameter
belongs to another module, beside different local declarations with the same
record and function names. Every callback body must be checked in its declaring
scope; a function signature alone is not proof of supported implementation.

`callbacks_false.av` passes its finite zero sample, but its intermediate claim is
false at nonzero inputs. `callbacks_unsupported.av` hides Float work in a named
Int-to-Int callback and must be declined. `callbacks_recursive.av` passes an
active function as a callback, introducing recursion that cannot be justified by
the direct-call graph; it must be declined and must never be VM-executed.

These fixtures use ordinary named function arguments only. They introduce no
arbitrary function-valued law givens, closures, function equality or local
function-value aliases.
