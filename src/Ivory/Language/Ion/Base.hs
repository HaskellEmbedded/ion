{- |
Module: Base
Description: Base Ion types
Copyright: (c) 2015 Chris Hodapp

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.Ion.Base where

import           Control.Exception
import           Control.Monad.State hiding ( forever )
import           Data.Typeable

import qualified Ivory.Language as IL
import           Ivory.Language
import qualified Ivory.Language.Monad as ILM

-- | This wraps 'Ion' with the ability to create unique C identifier names.
type Ion = State IonDef

-- | This wraps a pattern of functions calling each other in
-- continuation-passing style.  The intent is that the returned entry
-- function (which takes arguments 'a') causes the supplied
-- continuation function to be called (passing arguments 'b').
--
-- This is a common pattern for asynchronous calls, for instance, in
-- which the callback or interrupt calls the continuation function.
--
-- Multiple calls of this sort can be composed with '(<<=)' (and with
-- @RecursiveDo@ and 'mdo') to chain them in the order in which they
-- would proceed.
-- 
-- For instance, in @start <- call1 <<= call2 <<= call3 final@,
-- @start@ contains the entry function to @call1@, whose continuation
-- is set to the entry function of @call2@, whose continuation in turn
-- is set to the entry function of @call3@, whose continuation is
-- 'final'.  Note that chaining these with '(=<<)' is possible too,
-- but the order is somewhat reversed from what is logical - hence,
-- 'mdo' often being sensible here.
type IonCont a b = IL.Def (b ':-> ()) -- ^ Continuation function
                   -> Ion (Def (a ':-> ())) -- ^ Entry function

data IonDef = IonDef { ionId :: String -- ^ Unique ID (used as base name)
                     , ionNum :: Int -- ^ Next unused number
                     , ionDefs :: IL.ModuleDef -- ^ Ivory definitions
                       -- that the specifications produce
                     , ionCtxt :: Schedule -- ^ The 'inherited' context
                     , ionSched :: [Schedule] -- ^ A flat list of
                       -- schedule items generated along the way.
                     }

defaultIonDef = IonDef { ionId = ""
                       , ionNum = 0
                       , ionDefs = return ()
                       , ionCtxt = defaultSchedule
                       , ionSched = []
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

data IonException = InvalidCName [String] String Int -- ^ Path, C name, and
                    -- index at which it is invalid
                  | PhaseExceedsPeriod [String] Integer Integer -- ^ Path,
                    -- phase, period
                  | PhaseIsNegative [String] Integer -- ^ Path, phase
                  | PeriodMustBePositive [String] Integer -- ^ Path, period
    deriving (Show, Typeable)

instance Exception IonException
