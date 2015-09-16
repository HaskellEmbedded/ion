{- |
Module: Base
Description: Base Ion types
Copyright: (c) 2015 Chris Hodapp

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Language.Ion.Base where

import           Control.Exception
import           Control.Monad.State hiding ( forever )
import qualified Data.Tree as Tree
import           Data.Typeable

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM

-- | This wraps 'Ion' with the ability to create unique C identifier names.
type Ion = State IonDef

data IonDef = IonDef { ionId :: String -- ^ Unique ID (used as base name)
                     , ionNum :: Int -- ^ Next unused number
                     , ionDefs :: IL.ModuleDef -- ^ Ivory definitions
                                  -- that the specifications produce
                     , ionCtxt :: Schedule -- ^ The 'inherited' context
                     , ionTree :: [IonTree] -- ^ A tree of specifications
                     }

defaultIonDef = IonDef { ionId = ""
                       , ionNum = 0
                       , ionDefs = return ()
                       , ionCtxt = defaultSchedule
                       , ionTree = []
                       }

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

-- | A tree of commands, some of which apply hierarchically.  For instance,
-- setting a name ('SetName') adds a prefix the path to all branches
-- underneath; setting a period ('SetPeriod') will set the period of
-- all branches underneath, provided something else underneath does
-- not override.  ('modSchedule' contains the specific rules.)
type IonTree = Tree.Tree IonAction

-- | The type of Ivory action that an 'IonNode' can support. Note that this
-- purposely forbids breaking, returning, and allocating.
type IvoryAction = IL.Ivory IL.NoEffects

instance Show (IvoryAction a) where
  show iv = "Ivory NoEffects () [" ++ show block ++ "]"
    where (_, block) =
            ILM.runIvory $ ILM.noReturn $ ILM.noBreak $ ILM.noAlloc iv

data PhaseContext = Absolute -- ^ Phase is relative to the first tick
                             -- within a period
                  | Relative -- ^ Phase is relative to the last phase
                             -- used
                  deriving (Show)

data PhaseType = Min -- ^ Minimum phase (i.e. at this phase, or any
                     -- later point)
               | Exact -- ^ Exactly this phase
               deriving (Show)

-- | An action/effect that a node can have.
data IonAction = IvoryEff (IvoryAction ()) -- ^ The Ivory effects that this
                 -- node should perform
               | SetPhase PhaseContext PhaseType Integer -- ^ Setting phase -
                 -- i.e. the count within a period (thus, an absolute phase
                 -- must range from @0@ up to @N-1@ for period @N@).
               | SetPeriod Integer -- ^ Setting period
               | SetName String -- ^ Setting a name
               | AddCondition (IvoryAction IL.IBool) -- ^ Adding a condition to
                 -- this node which must return 'true' for the node *and* for
                 -- any sub-nodes to execute their actions
               | Disable -- ^ Disable this node and all children
               deriving (Show)

data IonException = InvalidCName [String] String Int -- ^ Path, C name, and
                    -- index at which it is invalid
                  | PhaseExceedsPeriod [String] Integer Integer -- ^ Path,
                    -- phase, period
    deriving (Show, Typeable)

instance Exception IonException
