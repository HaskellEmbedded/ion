{- |
Module: Ion
Description: Top-level Ion module
Copyright: (c) 2015 Chris Hodapp

Ion is a Haskell EDSL that is inspired by another EDSL,
<https://hackage.haskell.org/package/atom Atom>.  Ion aims to be a
re-implementation of Atom which, rather than generating C code directly (as
Atom does), interfaces with another very powerful, more general EDSL,
<http://ivorylang.org/ Ivory>.

To-do items:

   * I need to convert over the 'schedule' function in Scheduling.hs in Atom.
(This is partially done in 'flatten'.)
   * I can do a relative phase; what about a relative period? That is, a
period which is relative not to the base rate, but to the last rate that was
inherited.
   * I do not ever check that (absolute) phase @N@ follows @0 <= N <= P-1@,
where @P@ is the period.
   * Counterpart to 'cond' in Atom should compose as 'phase' and 'period' do.
(Is this necessary when I can just use the Ivory conditional?)
   * A combinator to explicitly disable a rule (also composing like 'cond')
might be nice too.
   * I need to either mandate that Ion names must be C identifiers, or make
a way to sanitize them into C identifiers.
   * Atom treats everything within a node as happening at the same time, and I
do not handle this yet, though I rather should.  This may be complicated - I
may either need to process the Ivory effect to look at variable references, or
perhaps add certain features to the monad.
   * Right now one can only pass variables to an Ion by way of a Ref or some
derivative, and those must then be dereferenced inside of an 'ivoryEff' call.
Is this okay?  Should we make this more flexible somehow?  (I feel like Atom
did it similarly, with V & E.)
   * In Atom you can declare a variable inside the monad, and that variable
declaration is carried around inside it.  Something similar may be good in Ion,
as it avoids the need for the user to manually 'defMemArea' and couple
definitions.  However, if I do this, how will I allow external Ivory code
access to the variable?

-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Ion where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State hiding ( forever )
import           Data.Maybe ( mapMaybe )
import           Data.Typeable

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM

-- | The monad for expressing an Ion specification.
data Ion a = Ion { ionNodes :: [IonNode] -- ^ An accumulation of nodes; the
                   -- head is considered the 'current' node
                 , ionVal :: a
                 } deriving (Show)

instance Functor Ion where
  fmap f ion = ion { ionVal = f $ ionVal ion }

instance Applicative Ion where
  pure = return
  (<*>) = ap

instance Monad Ion where
  ion1 >>= fn = ion2 { ionNodes = ionNodes ion2 ++ ionNodes ion1 }
    where ion2 = fn $ ionVal ion1

  return a = Ion { ionNodes = [], ionVal = a }

-- | A node representing some context in the schedule, and the actions this
-- node includes.  'ionAction' (except for 'IvoryEff' and 'NoAction') applies
-- not just to the current node, but to any child nodes too.  In general,
-- if two actions conflict (e.g. two 'SetPhase' actions with absolute phase),
-- then the innermost one overrides the other.
data IonNode = IonNode { ionAction :: IonAction -- ^ What this node does
                       , ionSub :: [IonNode] -- ^ Child nodes
                       } deriving (Show)

-- | The type of Ivory action that an 'IonNode' can support. Note that this
-- purposely forbids breaking, returning, and allocating.
type IvoryAction = IL.Ivory IL.NoEffects ()

instance Show IvoryAction where
  show iv = "Ivory NoEffects () [" ++ show block ++ "]"
    where (_, block) = ILM.runIvory $ ILM.noReturn $ ILM.noBreak $ ILM.noAlloc iv

-- | An action/effect that a node can have.
data IonAction = IvoryEff IvoryAction -- ^ The Ivory effects that this
                 -- node should perform
               | SetPhase PhaseContext PhaseType Int -- ^ Setting phase - i.e.
                 -- i.e. the count within a period (thus, an absolute phase
                 -- must range from @0@ up to @N-1@ for period @N@).
               | SetPeriod Int -- ^ Setting period
               | SetName String -- ^ Setting a name
               | NoAction -- ^ Do nothing
               deriving (Show)

-- | Phase = Phase PhaseContext PhaseType Int deriving (Show)

data PhaseContext = Absolute -- ^ Phase is relative to the first tick within a
                    -- period
                  | Relative -- ^ Phase is relative to the last phase used
                  deriving (Show)

data PhaseType = Min -- ^ Minimum phase (i.e. at this phase, or any later point)
               | Exact -- ^ Exactly this phase
               deriving (Show)

defaultNode = IonNode { ionAction = NoAction
                      , ionSub = []
                      }

-- | Produce a somewhat more human-readable representation of an 'Ion'
prettyPrint :: IonNode -> IO ()
prettyPrint node = putStrLn $ unlines $ pretty node
  where sub s = join $ map pretty $ ionSub s
        pretty s = [ "IonNode {"
                   , " ionAction = " ++ (show $ ionAction s)
                   ] ++
                   (if null $ ionSub s
                    then []
                    else " ionSub =" : (map ("    " ++) $ sub s)) ++
                   ["}"]

-- | Given a function which transforms an 'IonNode', and a child 'Ion', return
-- the parent 'Ion' containing this node with that transformation applied.
makeSub :: (IonNode -> IonNode) -> Ion a -> Ion a
makeSub fn ion0 = ion0
                  { ionNodes = [(fn defaultNode) { ionSub = ionNodes ion0 }] }

-- | Given an 'IonAction' to apply, and a child 'Ion', return the parent 'Ion'
-- containing this node with that action.
makeSubFromAction :: IonAction -> Ion a -> Ion a
makeSubFromAction act = makeSub (\i -> i { ionAction = act })

-- | Specify a name of a sub-node, returning the parent.
ion :: String -- ^ Name
       -> Ion a -- ^ Sub-node
       -> Ion a
ion = makeSubFromAction . SetName

-- | Specify a phase for a sub-node, returning the parent. (The sub-node may
-- readily override this phase.)
phase :: Int -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase = makeSubFromAction . SetPhase Absolute Min
-- FIXME: This needs to comprehend the different phase types.

delay :: Int -- ^ Relative phase
         -> Ion a -- ^ Sub-node
         -> Ion a
delay = makeSubFromAction . SetPhase Relative Min

-- | Specify a period for a sub-node, returning the parent. (The sub-node may
-- readily override this period.)
period :: Int -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period = makeSubFromAction . SetPeriod

-- | Turn an Ivory effect into an 'Ion'.
ivoryEff :: IvoryAction -> Ion ()
ivoryEff iv = Ion { ionNodes = [defaultNode { ionAction = IvoryEff iv }]
                  , ionVal = ()
                  }

-- | A scheduled action.  Phase and period here are absolute, and there are no
-- child nodes.
data Schedule = Schedule { schedId :: Integer -- ^ A unique ID for this
                           -- action
                         , schedName :: String -- ^ Name (without any
                           -- disambiguation applied)
                         , schedPath :: [String] -- ^ A list of names giving
                           -- the trail that produced this schedule
                         , schedPhase :: Integer -- ^ The (absolute & exact)
                           -- phase of this action
                         , schedPeriod :: Integer -- ^ The period of this
                           -- action
                         , schedAction :: [IvoryAction] -- ^ The Ivory effects
                           -- for this action
                         }
              deriving (Show)

defaultSchedule = Schedule { schedId = 0
                           , schedName = "root"
                           , schedPath = []
                           , schedPhase = 0
                           , schedPeriod = 1
                           , schedAction = []
                           }

-- | Transform a 'Schedule' according to an 'IonAction'.
modSchedule :: IonAction -> Schedule -> Schedule
modSchedule (IvoryEff _) s = s
modSchedule (SetPhase Absolute _ ph) s = s { schedPhase = fromIntegral ph }
modSchedule (SetPhase Relative _ ph) s =
  s { schedPhase = schedPhase s + fromIntegral ph }
modSchedule (SetPeriod p) s = s { schedPeriod = fromIntegral p }
modSchedule (SetName name) s = s { schedName = name
                                 , schedPath = schedPath s ++ [name]
                                 }
modSchedule NoAction s = s
-- FIXME: Handle exact and minimum phase.

-- | Actual 'State' implementation of 'flatten'; the contained state
-- is (starting schedule state, accumulated schedule).
flattenSt :: IonNode -> State (Schedule, [Schedule]) ()
flattenSt node = do
  (ctxt, scheds) <- get
  let ctxt' = modSchedule (ionAction node) ctxt
      -- For the context that we pass forward, clear out old actions (they run
      -- only once) and produce a fresh ID:
      ctxtNext = ctxt' { schedAction = [], schedId = schedId ctxt + 1 }
      -- Get a unique name:
      name = schedName ctxt' ++ "_" ++ (show $ schedId ctxt')
      -- Get Ivory actions (if any) or else Nothing:
      getIvory node = case ionAction node of IvoryEff iv -> Just iv
                                             _           -> Nothing
  -- Emit schedule nodes for any children that have Ivory effects (We do this
  -- to combine all effects at once that are under the same parameters.)
  case (mapMaybe getIvory $ ionSub node) of
   []      -> put (ctxtNext, scheds)
   actions -> put (ctxtNext, newSched : scheds)
    where newSched = ctxt' {schedAction = actions, schedName = name}
  -- And recurse to the sub-nodes!
  mapM_ flattenSt $ ionSub node
-- FIXME: This does not handle exact or relative phase.

-- | Walk a hierarchical 'IonNode' and turn it into a flat list of
-- scheduled action.
flatten :: IonNode -> [Schedule]
flatten node = l
  where (_, l) = execState (flattenSt node) (defaultSchedule, [])

data IonException = NodeUnboundException IonNode
    deriving (Show, Typeable)

instance Exception IonException
