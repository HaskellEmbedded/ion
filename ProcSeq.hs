{- |
Module: ProcSeq
Description: Sequenced/bundled procedures
Copyright: (c) 2015 Chris Hodapp

This is an experimental module for sequencing and bundling related procedures
together, particularly for cases when they call each other in
continuation-passing style or rely on asynchronous callbacks - and need to be
composed.

I am still not entirely sure how much this belongs in Ion.

Things to consider:

   * How would I represent a long non-blocking delay in this?
   * In my SPI example, I send an opcode along with a length and an expected
length to read back.  This call is async, and the return continuation receives
the number of bytes actually read.  I want to check this number at the return
continuation - and I'd like to avoid having to write this manually for every
case and repeat the number of expected bytes.  How would I represent this?

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ProcSeq ( ProcSeq
               , seqDef
               , newProc
               , newProcP
               , newArea
               , newAreaP
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

import           Control.Applicative ( Const )
import           Control.Monad.State hiding ( forever )
import           Data.Proxy ( Proxy )

import           Ivory.Language hiding ( Proxy )
import           Ivory.Language.MemArea
import           Ivory.Language.Proc ( Def(..), IvoryCall_, IvoryProcDef )

-- | This type assists with bundling together sequences of call and state,
-- while creating unique names so that multiple instances do not conflict.
type ProcSeq t = State SeqState t

-- | State that is passed along in a 'ProcSeq' which accumulates 'ModuleDef'
-- and increments numbers to generate unique names
data SeqState = SeqState { seqId :: String -- ^ Unique (per instance) ID
                         , seqNum :: Int -- ^ Number that is incremented
                         , seqDefs :: ModuleDef
                         }

-- | Return the procedure for a 'ProcSeq' and the acculumated 'ModuleDef',
-- given a unique string for an ID.
seqDef :: ProcSeq (Def proc) -> String -> (Def proc, ModuleDef)
seqDef s id = (fn, seqDefs st)
  where (fn, st) = runState s init
        init = SeqState { seqId = id, seqNum = 0, seqDefs = return () }

-- | Like Ivory 'proc', but leaving out the first argument (it derives the
-- name from 'ProcSeq').
newProc :: (IvoryProcDef proc impl) => impl -> ProcSeq (Def proc)
newProc impl = do
  state <- get
  let num' = (seqNum state) + 1
      fn = proc (seqId state ++ "_" ++ show num') impl
      state' = state { seqNum = num'
                     , seqDefs = seqDefs state >> incl fn
                     }
  put state'
  return fn

-- | 'newProc' with an initial 'Proxy' to disambiguate the procedure type
newProcP :: (IvoryProcDef proc impl) =>
            Proxy (Def proc) -> impl -> ProcSeq (Def proc)
newProcP _ = newProc

-- | Like Ivory 'area', but leaving out the first argument (it derives the
-- name from 'ProcSeq').
newArea :: (IvoryArea area, IvoryZero area) =>
           Maybe (Init area) -> ProcSeq (MemArea area)
newArea init = do
  state <- get
  let num' = (seqNum state) + 1
      a = area (seqId state ++ "_" ++ show num') init
      state' = state { seqNum = num'
                     , seqDefs = seqDefs state >> defMemArea a
                     }
  put state'
  return a

-- | 'newArea' with an initial 'Proxy' to disambiguate the area type
newAreaP :: (IvoryArea area, IvoryZero area) =>
            Proxy area -> Maybe (Init area) -> ProcSeq (MemArea area)
newAreaP _ = newArea

-- | Create a timer resource.  The returned 'Ion' still must be called at
-- regular intervals (e.g. by including it in a larger Ion spec that is
-- already active).  See 'startTimer' and 'endTimer' to actually activate this
-- timer.
--timer :: Def ('[] :-> ()) -- ^ Timer expiration function
--         -> ProcSeq (Ion ())

-- | All the functions below are for generating procedures to adapt a procedure
-- of different numbers of arguments.  I am almost certain that a better way
-- exists than what I did below - probably using typeclasses and mimicking
-- what Ivory did to define the functions.
adapt_0_1 :: (IvoryType a, IvoryVar a) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a] ':-> ()))
adapt_0_1 fn0 = newProc $ \_ -> body $ call_ fn0

adapt_1_0 :: (Num a, IvoryType a, IvoryVar a) =>
             Def ('[a] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_1_0 fn0 = newProc $ body $ call_ fn0 0

adapt_0_2 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b] ':-> ()))
adapt_0_2 fn0 = newProc $ \_ _ -> body $ call_ fn0

adapt_2_0 :: (Num a, IvoryType a, IvoryVar a, Num b, IvoryType b, IvoryVar b) =>
             Def ('[a, b] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_2_0 fn0 = newProc $ body $ call_ fn0 0 0

adapt_0_3 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b, IvoryType c,
              IvoryVar c) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b,c] ':-> ()))
adapt_0_3 fn0 = newProc $ \_ _ _ -> body $ call_ fn0

adapt_3_0 :: (Num a, IvoryType a, IvoryVar a, Num b, IvoryType b, IvoryVar b,
              Num c, IvoryType c, IvoryVar c) =>
             Def ('[a, b, c] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_3_0 fn0 = newProc $ body $ call_ fn0 0 0 0

adapt_0_4 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b, IvoryType c,
              IvoryVar c, IvoryType d, IvoryVar d) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b,c,d] ':-> ()))
adapt_0_4 fn0 = newProc $ \_ _ _ _ -> body $ call_ fn0

adapt_4_0 :: (Num a, IvoryType a, IvoryVar a, Num b, IvoryType b, IvoryVar b,
              Num c, IvoryType c, IvoryVar c, Num d, IvoryType d, IvoryVar d) =>
             Def ('[a, b, c, d] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_4_0 fn0 = newProc $ body $ call_ fn0 0 0 0 0

adapt_0_5 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b, IvoryType c,
              IvoryVar c, IvoryType d, IvoryVar d, IvoryType e, IvoryVar e) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b,c,d,e] ':-> ()))
adapt_0_5 fn0 = newProc $ \_ _ _ _ _ -> body $ call_ fn0
