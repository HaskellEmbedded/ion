{- |
Module: Ion
Description: Top-level Ion module
Copyright: (c) 2015 Chris Hodapp

Ion is a Haskell EDSL that is inspired by another EDSL,
<https://hackage.haskell.org/package/atom Atom>.  Ion aims to be a
re-implementation of Atom which, rather than generating C code
directly (as Atom does), interfaces with another very powerful, more
general EDSL, <http://ivorylang.org/ Ivory>.

It also contains support for sequencing and bundling related
procedures together, particularly for cases when they call each other
in continuation-passing style or rely on asynchronous callbacks - and
need to be composed.

To-do items:

   * Continue writing documentation and examples!
   * Get some unit tests for things that I am prone to breaking.
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
    Ion
  
    -- * Code generation
  , IonExports(..)
  , ionDef

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
  , ion
  , phase
  , delay
  , period
  , subPeriod
  , cond
  , disable
    
    -- ** Memory & Procedures
  , newName
  , newProc
  , newProcP
  , area'
  , areaP'
  , newArea
  , newAreaP
    
    -- ** Effects
  , ivoryEff
    
    -- ** Utilities
  , timer
  , startTimer
  , stopTimer
  , getPhase
  , adapt_0_1
  , adapt_1_0
  , adapt_0_2
  , adapt_2_0
  , adapt_0_3
  , adapt_3_0
  , adapt_0_4
  , adapt_4_0
  , adapt_0_5

  ) where

import Ivory.Language.Ion.Base
import Ivory.Language.Ion.Code
import Ivory.Language.Ion.Example
import Ivory.Language.Ion.Operators
import Ivory.Language.Ion.Util
