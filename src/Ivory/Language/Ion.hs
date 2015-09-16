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

   * Put this code into sensible namespaces!
   * Continue writing documentation and examples!
   * I need to convert over the 'schedule' function in Scheduling.hs in Atom.
(This is partially done in 'flatten'.)
   * I can do a relative phase; what about a relative period? That is, a
period which is relative not to the base rate, but to the last rate that was
inherited.
   * Atom treats everything within a node as happening at the same time, and I
do not handle this yet, though I rather should.  This may be complicated - I
may either need to process the Ivory effect to look at variable references, or
perhaps add certain features to the monad.
   * Right now one can only pass variables to an Ion by way of a Ref or some
derivative, and those must then be dereferenced inside of an 'ivoryEff' call.
Is this okay?  Should we make this more flexible somehow?  (I feel like Atom
did it similarly, with V & E.)
   * Pretty-printing the schedule itself (as Atom does) would probably be a
good idea.
   * Consider the case where you put a condition on a node, and that node
has many sub-nodes across various delays.  Now, suppose that that condition
becomes false somewhere in the middle of those delays.  Is the entire node
blocked from taking effect, or does it partially take effect?  When is the
condition considered as being evaluated?  Right now it is evaluated at every
single sub-node that inherits it.  I consider this to be a violation of how
Ion should operate - synchronously and atomically.

Things to consider (copied from ProcSeq):

   * How would I represent a long non-blocking delay in this?
   * It's still a bit cumbersome when combining together Ivory procedures of
different types, though my 'adapt_x_y' calls help somewhat.
   * This is done, but needs a good example:
In my SPI example, I send an opcode along with a length and an expected
length to read back.  This call is async, and the return continuation receives
the number of bytes actually read.  I want to check this number at the return
continuation - and I'd like to avoid having to write this manually for every
case and repeat the number of expected bytes.  How would I represent this?

-}

module Ivory.Language.Ion (
    -- * Base types
    Ion
  
    -- * Code generation
  , IonExports(..)
  , ionDef

    -- * Operators
  , ion
  , cond
  , period
  , getPhase
  , phase
  -- Disabled for now because it's problematic:
  -- , delay
  , ivoryEff
  , timer
  , newName
  , newProc
  , newProcP
  , area'
  , areaP'
  , newArea
  , newAreaP
  , startTimer
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
