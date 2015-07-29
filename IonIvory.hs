{- |
Module: IonIvory
Description: Conversion from Ion to Ivory modules & procedures
Copyright: (c) 2015 Chris Hodapp

This contains functionality for converting the 'Ion' type to Ivory constructs.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module IonIvory where

import           Control.Exception
import           Control.Monad.State.Lazy

import           Ivory.Language

import           Ion

-- | Generate an Ivory module from the given Ion spec.
ionModule :: Ion () -> Module
ionModule i0 = package "ion" $ do
  mapM_ incl $ ionProc i0

-- | Generate Ivory procedures for the given Ion spec.
ionProc :: Ion () -> [Def ('[] :-> ())]
ionProc i0 = map mkProc $ flatten $ execState i0 defaultNode
  where mkProc node = proc ("ion_" ++ ionName node) $ body $ do
          noReturn $ noBreak $ noAlloc $ getIvory node
-- This perhaps should be seen as an analogue of 'writeC' in Code.hs in Atom.

-- | Produce an Ivory effect from an 'IonNode'.
getIvory :: (eff ~ NoEffects) => IonNode -> Ivory eff ()
-- Originally:
-- (GetBreaks eff ~ NoBreak, GetReturn eff ~ NoReturn, GetAlloc eff ~ NoAlloc)
getIvory i0 = do
  comment $ "Node: " ++ (foldl1 (\s p -> s ++ "." ++ p) $ ionPath i0)
  comment $ "Period: " ++ (show $ ionPeriod i0)
  comment $ "Phase: " ++ (show $ ionPhase i0)
  case (ionAction i0) of
   Nothing -> comment "No actions"
   Just a -> do
     --when (ionUnbound i0) $ throw $ NodeUnboundException i0
     when (ionUnbound i0) $ comment $ "WARNING: Unbound params"
     a
  let numSubs = length $ ionSub i0
  when (numSubs > 0) $ do
    comment $ "WARNING: Ignored " ++ show numSubs ++ " sub-nodes"

-- The main Atom function flattens everything, and this I should probably do
-- as well.  It follows the pattern of:
-- {
--    if (...) {
--       call out...
--    } else {
--       increment clock
-- }

-- I am generating functions, but I am not yet calling them.
