{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IonWriter where

import           Control.Exception
import           Control.Monad.Writer
import qualified Data.Tree as Tree
import           Data.Typeable

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM
import           Ivory.Language.Proc ( Def(..), Proc(..), IvoryCall_,
                                       IvoryProcDef )

import           IonUtil

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

type IonTree = Tree.Tree IonAction

type Ion = Writer [IonTree]

addAction :: IonAction -> Ion a -> Ion a
addAction act = mapWriter f
  where f (a, nodes) = (a, [Tree.Node act nodes])

period :: Integer -> Ion a -> Ion a
period = addAction . SetPeriod

phase :: Integer -> Ion a -> Ion a
phase = addAction . SetPhase Absolute Exact

delay :: Integer -> Ion a -> Ion a
delay = addAction . SetPhase Relative Exact

ion :: String -> Ion a -> Ion a
ion = addAction . SetName

cond :: (IvoryAction IL.IBool) -> Ion a -> Ion a
cond = addAction . AddCondition

disable :: Ion a -> Ion a
disable = addAction Disable

ivoryEff :: (IvoryAction ()) -> Ion a -> Ion a
ivoryEff = addAction . IvoryEff

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
modSchedule (IvoryEff _) s = s
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

flattenTree :: Schedule -> IonTree -> [Schedule]
flattenTree ctxt (Tree.Node action forest) = this : rest
  where this = modSchedule action ctxt
        rest = join $ map (flattenTree this) forest

flatten :: Ion () -> [Schedule]
flatten i = join $ map (flattenTree defaultSchedule) $ execWriter i

prune :: [Schedule] -> [Schedule]
prune = filter undefined

data IonException = InvalidCName [String] String Int -- ^ Path, C name, and
                    -- index at which it is invalid
                  | PhaseExceedsPeriod [String] Integer Integer -- ^ Path,
                    -- phase, period
    deriving (Show, Typeable)

instance Exception IonException

test :: Ion ()
test = do
  ion "foo" $ period 20 $ ion "bar" $ do
    ion "baz" $ return ()
    ion "quux" $ return ()
    period 10 $ ion "period10" $ period 5 $ return ()
    period 10 $ ion "period10b" $ return ()
    return ()
  period 40 $ do
    return ()
  return ()
