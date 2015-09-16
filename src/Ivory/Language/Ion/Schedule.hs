{- |
Module: Schedule
Description: Types and functions for flattened schedule
Copyright: (c) 2015 Chris Hodapp

-}
module Ivory.Language.Ion.Schedule where

import qualified Ivory.Language as IL

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Util

-- | Produce a flat list of scheduled actions.
flatten :: IonDef -> [Schedule]
flatten i = uniqueIds 0 $ prune $ ionSched i
            -- join $ map (flattenTree defaultSchedule) $ ionTree i

-- | Prune any schedule item that has no Ivory actions.
prune :: [Schedule] -> [Schedule]
prune = filter (not . null . schedAction)

-- | Assign unique IDs to the list of schedule items, starting from the given
-- ID.
uniqueIds :: Integer -> [Schedule] -> [Schedule]
uniqueIds _ [] = []
uniqueIds n (x:xs) = (x { schedId = n }) : uniqueIds (n + 1) xs
