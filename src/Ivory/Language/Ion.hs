{- |
Module: Ion
Description: Top-level Ion module
Copyright: (c) 2015 Chris Hodapp

Ion is a Haskell EDSL for concurrent, realtime, embedded programming.
It performs compile-time scheduling, and produces scheduling code
with constant memory usage and deterministic execution (i.e. no
possibility for divergence).

It interfaces with another, more powerful EDSL, <http://ivorylang.org/
Ivory>, to perform code generation.  Ivory is responsible for all the
code generation to perform the scheduling.  One may also embed general
Ivory effects in an Ion spec with few restrictions, however, it does
very little to enforce constant memory usage or deterministic code
here.

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
purpose of these is to allow that same compositional when working with
Ivory definitions that are parametrized and may be instantiated
multiple times.

For instance, when dealing with functions that return via asynchronous
callbacks or interrupts - a common thing on embedded systems - one
must generally work in continuation-passing style.  This simplifies
the process of creating a reusable pattern for a use-case like:

1. Transmit instruction @I@ over SPI. Wait to receive 2 bytes.
2. In a callback: Check that result for being an error condition.  If
an error, call error handler function @E@.  If successful, transmit
instruction @I2@ and wait to receive 2 bytes.
3. In a callback: Check for error and call @E@ if needed.  If successful,
combine result into some composite value, and call success handler @S@
with that value.

and then parametrizing this whole definition over instructions @I@ and
@I2@, error handler @E@, and success handler @S@.  This definition
then could be parametrized over multiple different instructions, and
all of these chained together (e.g. via @(=<<)@) to create a larger
sequence of calls passing control via CPS.

Ion was heavily inspired by another EDSL,
<https://hackage.haskell.org/package/atom Atom>. It started as an Atom
re-implementation which had other backends, rather than generating C
code directly (as Atom does).  However, Ion has diverged somewhat, and
still does not have many things from Atom, such as synchronous
variable access, run-time checks on execution time, various
compile-time sanity checks, traces, or most of its standard library.

To-do items:

   * Continue writing documentation and examples!
   * Get some unit tests for things that I am prone to breaking.
   * It *still* does not handle 'minimum' phase.
   * This could use a way to 'invert' a phase, and run at every phase but
the ones noted.
   * I need to convert over the 'schedule' function in Scheduling.hs in Atom.
   * Atom treats everything within a node as happening at the same time, and I
do not handle this yet, though I rather should.  This may be complicated - I
may either need to process the Ivory effect to look at variable references, or
perhaps add certain features to the monad.
   * Atom had a way to express things like rising or falling edges, and
debouncing.  How possible is this to express?
   * Right now one can only pass variables to an Ion by way of a Ref or some
derivative, and those must then be dereferenced inside of an 'ivoryEff' call.
Is this okay?  Should we make this more flexible somehow?  (I feel like Atom
did it similarly, with V & E.)
   * Pretty-printing the schedule itself (as Atom does) would probably be a
good idea.
   * Consider the case where one puts a condition on a node, and that node
has many sub-nodes across various delays.  Now, suppose that that condition
becomes false somewhere in the middle of those delays.  Is the entire node
blocked from taking effect, or does it partially take effect?  When is the
condition considered as being evaluated?  Right now it is evaluated at every
single sub-node that inherits it.  I consider this to be a violation of how
Ion should operate - synchronously and atomically.
   * Could 'ivoryEff' meaningfully return a value to 'Ion' rather than ()?
   * Would it be possible to make a CFG for the continuation-passing style
arrangements?  (Might Ivory have to handle this?)
   * Runtime check: Schedule function being called twice in one clock tick.
   * Runtime check: Schedule function never called in a clock tick.
   * Runtime check: Schedule function hasn't returned yet when next clock
tick occurs (i.e. schedule function takes too long).
   * Runtime check: Compute percent utilization, time-wise, in schedule
function.
   * Compile-time check: Same period and phase occupied.  (Atom would throw
a compile-time error when this happened.)

-}

module Ivory.Language.Ion (
    -- * Base types
    Base.Ion
  , CPS.IonCont
  
    -- * Code generation
  , Code.IonExports(..)
  , Code.ionDef

    -- * Operators
    
    -- ** Compositional
    -- | These functions all have @'Ion' a -> 'Ion' a@ (or similar) at the
    -- end of their type, and that is because they are meant to be
    -- nested by function composition. For instance:
    --
    -- @
    -- 'ion' "top_level" $ do
    --     'ion' "sub_spec" $ 'period' 100 $ do
    --          'ion' "phase0" $ 'phase' 0 $ do
    --              -- Everything here inherits period 100, phase 0, and
    --              -- a new path "top_level.sub_spec.phase0".
    --          'phase' 20 $ 'phase' '30' $ do
    --              -- Everything here inherits period 100, and phase 30
    --          'phase' 40 $ 'cond' (return true) $ do
    --              -- Everything here inherits period 100, phase 40, and
    --              -- a (rather vacuous) condition
    --          'disable' $ 'phase' 50 $ do
    --              -- This is all disabled.
    -- @
    --
    -- Note that more inner bindings override outer ones in the case
    -- of 'phase', 'delay', 'period', and 'subPeriod'.  Applications
    -- of 'cond' combine with each other as a logical @and@.
    -- Applications of 'disable' are idempotent.
  , Operators.ion
  , Operators.phase
  , Operators.delay
  , Operators.period
  , Operators.subPeriod
  , Operators.cond
  , Operators.disable
    
    -- ** Memory & Procedures
  , Operators.newName
  , Operators.newProc
  , Operators.newProcP
  , Operators.area'
  , Operators.areaP'
  , Operators.newArea
  , Operators.newAreaP
    
    -- ** Effects
  , Operators.ivoryEff
    
    -- ** Utilities
  , Operators.timer
  , Operators.startTimer
  , Operators.stopTimer
  , Operators.getPhase
  , Operators.adapt_0_1
  , Operators.adapt_1_0
  , Operators.adapt_0_2
  , Operators.adapt_2_0
  , Operators.adapt_0_3
  , Operators.adapt_3_0
  , Operators.adapt_0_4
  , Operators.adapt_4_0
  , Operators.adapt_0_5
    -- Yes, the 'utilities' aren't in module Util. Whatever.

    -- ** CPS
  , CPS.accum
    
  ) where

import qualified Ivory.Language.Ion.Base as Base
import qualified Ivory.Language.Ion.Code as Code
import qualified Ivory.Language.Ion.CPS as CPS
import qualified Ivory.Language.Ion.Example as Example
import qualified Ivory.Language.Ion.Operators as Operators
import qualified Ivory.Language.Ion.Util as Util
