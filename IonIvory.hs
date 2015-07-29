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

import           Control.Monad.State.Lazy

import           Ivory.Language

import           Ion

-- | Generate an Ivory module from the given Ion spec.
ionModule :: Ion () -> Module
ionModule i0 = package "ion" $ do
  incl $ ionProc i0

-- | Generate an Ivory procedure for the given Ion spec.
ionProc :: Ion () -> Def ('[] :-> ())
ionProc i0 = proc "ionProc" $ body $ do
  noReturn $ noBreak $ noAlloc $ getIvory $ execState i0 defaultNode
-- This perhaps should be seen as an analogue of 'writeC' in Code.hs in Atom.

-- | Produce an Ivory effect from an 'IonNode'.
getIvory :: (eff ~ NoEffects) => IonNode -> Ivory eff ()
-- Originally:
-- (GetBreaks eff ~ NoBreak, GetReturn eff ~ NoReturn, GetAlloc eff ~ NoAlloc)
getIvory i0 = do
  -- ifte_ present only to illuminate what's what
  ifte_ true
    (do
        comment $ "name = " ++ (show $ ionName i0)
        comment $ "path = " ++ (show $ ionPath i0)
        comment $ "period = " ++ (show $ ionPeriod i0)
        comment $ "phase = " ++ (show $ ionPhase i0)
        ionAction i0
        mapM_ getIvory $ ionSub i0)
    $ return ()
