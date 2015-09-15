{- |
Module: Schedule
Description: Types and functions for flattened schedule
Copyright: (c) 2015 Chris Hodapp

-}
module Ivory.Language.Ion.Schedule where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Writer
import qualified Data.Tree as Tree

import qualified Ivory.Language as IL

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Util

-- | A scheduled action.  Phase and period here are absolute, and there are no
-- child nodes.
data Schedule =
  Schedule { schedId :: Integer -- ^ A unique ID for this action
           , schedName :: String -- ^ Name (without any disambiguation applied)
           , schedPath :: [String] -- ^ A list of names giving the trail that
             -- produced this schedule
           , schedPhase :: Integer -- ^ The (absolute & exact) phase of this
             -- action
           , schedPeriod :: Integer -- ^ The period of this action
           , schedAction :: [IvoryAction ()] -- ^ The Ivory effects for this
                            -- action
           , schedCond :: [IvoryAction IL.IBool] -- ^ Ivory effects which all
                          -- must return 'true' for anything in 'schedAction'
                          -- to execute
           }
  deriving (Show)

defaultSchedule = Schedule { schedId = 0
                           , schedName = "root"
                           , schedPath = []
                           , schedPhase = 0
                           , schedPeriod = 1
                           , schedAction = []
                           , schedCond = []
                           }

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

-- | Produce a flat list of scheduled actions from an 'IonM'.
flatten :: IonM a -> [Schedule]
flatten i = uniqueIds 0 $ prune $ join $
            map (flattenTree defaultSchedule) $ ionTree $ execWriter i

-- | Prune any schedule item that has no Ivory actions.
prune :: [Schedule] -> [Schedule]
prune = filter (not . null . schedAction)

-- | Assign unique IDs to the list of schedule items, starting from the given
-- ID.
uniqueIds :: Integer -> [Schedule] -> [Schedule]
uniqueIds _ [] = []
uniqueIds n (x:xs) = (x { schedId = n }) : uniqueIds (n + 1) xs
