{- |
Module: ProcSeq
Description: Sequenced/bundled procedures
Copyright: (c) 2015 Chris Hodapp

This is an experimental module for sequencing and bundling related procedures
together, particularly for cases when they call each other in
continuation-passing style and need to be composed.

I am still not entirely sure how much this belongs in Ion.

-}
module ProcSeq ( ProcSeq
               , seqDef
               , newProc
               , newProcP
               , newArea
               , newAreaP
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
