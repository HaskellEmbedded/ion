{- |
Module: Schedule
Description: Types and functions for flattened schedule
Copyright: (c) 2015 Chris Hodapp

-}
module Ivory.Language.Ion.Schedule where

import           Control.Exception
import           Control.Monad
import qualified Data.Tree as Tree

import qualified Ivory.Language as IL

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Util

-- | Transform a 'Schedule' according to an 'IonAction'.
modSchedule :: IonAction -> Schedule -> Schedule
modSchedule (IvoryEff eff) s = s { schedAction = schedAction s ++ [eff] }
modSchedule (SetPhase t _ ph) s =
  if (ph' >= schedPeriod s)
  then throw $ PhaseExceedsPeriod (schedPath s) ph' (schedPeriod s)
  else s { schedPhase = ph' }
  where ph' = case t of Absolute -> ph
                        Relative -> schedPhase s + ph
modSchedule (SetPeriod p) s = s { schedPeriod = fromIntegral p }
modSchedule (SetName name) s =
  case checkCName name of Just i -> throw $ InvalidCName (schedPath s) name i
                          Nothing -> s { schedName = name
                                       , schedPath = schedPath s ++ [name]
                                       }
modSchedule (AddCondition iv) s = s { schedCond = iv : schedCond s }
-- FIXME: Handle exact and minimum phase.

-- | Recursive helper call to 'flatten'; first argument is a starting
-- context.
flattenTree :: Schedule -> IonTree -> [Schedule]
flattenTree ctxt (Tree.Node action forest) = this : rest
  where this = modSchedule action ctxt
        rest = join $ map (flattenTree this) forest

-- | Produce a flat list of scheduled actions.
flatten :: IonDef -> [Schedule]
flatten i = uniqueIds 0 $ prune $ join $
            map (flattenTree defaultSchedule) $ ionTree i

-- | Prune any schedule item that has no Ivory actions.
prune :: [Schedule] -> [Schedule]
prune = filter (not . null . schedAction)

-- | Assign unique IDs to the list of schedule items, starting from the given
-- ID.
uniqueIds :: Integer -> [Schedule] -> [Schedule]
uniqueIds _ [] = []
uniqueIds n (x:xs) = (x { schedId = n }) : uniqueIds (n + 1) xs
