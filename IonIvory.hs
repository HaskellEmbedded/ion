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

import           Ivory.Language

import           Ion

-- | Generate an Ivory module from the given Ion spec.
ionModule :: Ion () -> Module
ionModule i0 = package "ion" $ do
  let nodes = flatten defaultSchedule $ head $ ionNodes i0
  mapM_ incl $ ionProc nodes

-- | Generate Ivory procedures for the given Ion spec.
ionProc :: [Schedule] -> [Def ('[] :-> ())]
ionProc scheds = entryProc : schedProcs
  where schedProcs = map mkProc scheds
        mkProc sch = proc ("ion_" ++ schedName sch) $ body $ do
          noReturn $ noBreak $ noAlloc $ getIvory sch
        entryProc = proc "start_ion_" $ body $ do
          mapM_ call_ schedProcs
          -- TODO: Disambiguate the name of this procedure
-- This perhaps should be seen as an analogue of 'writeC' in Code.hs in Atom.

-- | Produce an Ivory effect from an 'IonNode'.
getIvory :: (eff ~ NoEffects) => Schedule -> Ivory eff ()
-- Originally:
-- (GetBreaks eff ~ NoBreak, GetReturn eff ~ NoReturn, GetAlloc eff ~ NoAlloc)
getIvory i0 = do
  comment $ "Name: " ++ schedName i0
  comment $ "Path: " ++ (foldl1 (\s acc -> (s ++ "." ++ acc)) $ schedPath i0)
  comment $ "Phase: " ++ (show $ schedPhase i0)
  comment $ "Period: " ++ (show $ schedPeriod i0)
  sequence_ $ schedAction i0
