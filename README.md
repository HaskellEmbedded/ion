# Ion

Ion is a (heavily experimental) Haskell EDSL for concurrent, realtime,
embedded programming.  It performs compile-time scheduling, and
produces scheduling code with constant memory usage and deterministic
execution (i.e. no possibility for divergence).

It interfaces with another, more powerful EDSL,
[Ivory](http://ivorylang.org/), to perform code generation.  Ivory is
responsible for all the code generation to perform the scheduling.
One may also embed general Ivory effects in an Ion spec with few
restrictions, however, it does very little to enforce constant memory
usage or deterministic code here.

Ion generates scheduling code which must be called at regular clock
ticks (i.e. from a timer interrupt).  The interval of these clock
ticks establishes the *base rate* of the system.  All scheduled events
in the system take place relative to this base rate, defined in terms
of 'period' (interval of repetition) and 'phase' (position within that
interval).

This functionality is expressed in the 'Ion' monad - in large part to
allow composition and modularity in expressing tightly-scheduled
functionality.  In addition, it has functions like 'newProc' and
'newArea' which define uniquely-named C functions and globals.  The
purpose of these is to allow that same composition when working with
Ivory definitions that are parametrized and may be instantiated
multiple times.

For instance, when dealing with functions that return via asynchronous
callbacks or interrupts - a common thing on embedded systems - one
must generally work in continuation-passing style.  This simplifies
the process of creating a reusable pattern for a use-case like:

1. Transmit instruction `I` over SPI. Wait to receive 2 bytes.
2. In a callback: Check that result for being an error condition.  If
an error, call error handler function `E`.  If successful, transmit
instruction `I2` and wait to receive 2 bytes.
3. In a callback: Check for error and call `E` if needed.  If successful,
combine result into some composite value, and call success handler `S`
with that value.

and then parametrizing this whole definition over instructions `I` and
`I2`, error handler `E`, and success handler `S`.  This definition
then could be parametrized over multiple different instructions, and
all of these chained together (e.g. via `(=<<)`) to create a larger
sequence of calls passing control via CPS.

Ion was heavily inspired by another EDSL,
[Atom](https://hackage.haskell.org/package/atom). It started as an
Atom re-implementation which had other backends, rather than
generating C code directly (as Atom does).  However, Ion has diverged
somewhat, and still does not have many things from Atom, such as
synchronous variable access, run-time checks on execution time,
various compile-time sanity checks, traces, or most of its standard
library.

[![Build Status](https://travis-ci.org/HaskellEmbedded/ion.svg?branch=master)](https://travis-ci.org/HaskellEmbedded/ion)
