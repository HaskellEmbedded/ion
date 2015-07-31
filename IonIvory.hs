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
  let nodes = flatten $ head $ ionNodes i0
  mapM_ incl $ ionProc nodes

-- | Generate Ivory procedures for the given Ion spec.
ionProc :: [IonNode] -> [Def ('[] :-> ())]
ionProc nodes = map mkProc nodes
  where mkProc node = proc ("ion_" ++ "undefined") $ body $ do
          noReturn $ noBreak $ noAlloc $ getIvory node
-- This perhaps should be seen as an analogue of 'writeC' in Code.hs in Atom.

-- | Produce an Ivory effect from an 'IonNode'.
getIvory :: (eff ~ NoEffects) => IonNode -> Ivory eff ()
-- Originally:
-- (GetBreaks eff ~ NoBreak, GetReturn eff ~ NoReturn, GetAlloc eff ~ NoAlloc)
getIvory i0 = do
  comment $ "Action: " ++ (show $ ionAction i0)
  case ionAction i0 of IvoryEff iv -> iv
                       _           -> return ()
  -- sequence_ $ ionAction i0

-- The main Atom function flattens everything, and this I should probably do
-- as well.  It follows the pattern of:
-- {
--    if (...) {
--       call out...
--    } else {
--       increment clock
-- }

-- I am generating functions, but I am not yet calling them.
