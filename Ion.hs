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

   * Continue writing documentation and examples!
   * I need to convert over the 'schedule' function in Scheduling.hs in Atom.
(This is partially done in 'flatten'.)
   * I can do a relative phase; what about a relative period? That is, a
period which is relative not to the base rate, but to the last rate that was
inherited.
   * Atom treats everything within a node as happening at the same time, and I
do not handle this yet, though I rather should.  This may be complicated - I
may either need to process the Ivory effect to look at variable references, or
perhaps add certain features to the monad.
   * Right now one can only pass variables to an Ion by way of a Ref or some
derivative, and those must then be dereferenced inside of an 'ivoryEff' call.
Is this okay?  Should we make this more flexible somehow?  (I feel like Atom
did it similarly, with V & E.)
   * Pretty-printing the schedule itself (as Atom does) would probably be a
good idea.
   * Replacing the existing Ion monad with some kind of free monad might
simplify and clarify the code.
   * Atom contained a way to retrieve the current period and phase inside the
monad; I should implement this.

-}
{-# LANGUAGE DataKinds #-}
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

import           IonUtil

-- | The monad for expressing an Ion specification.
data Ion a = Ion { ionNodes :: [IonNode] -- ^ An accumulation of nodes; the
                   -- head is considered the 'current' node
                 , ionDefs :: IL.ModuleDef -- ^ 'ModuleDef' for anything that
                   -- needs to be declared for Ivory
                 , ionVal :: a
                 }-- deriving (Show)

instance Functor Ion where
  fmap f ion = ion { ionVal = f $ ionVal ion }

instance Applicative Ion where
  pure = return
  (<*>) = ap

instance Monad Ion where
  ion1 >>= fn = ion2 { ionNodes = ionNodes ion1 ++ ionNodes ion2
                     , ionDefs = ionDefs ion1 >> ionDefs ion2
                     }
    where ion2 = fn $ ionVal ion1

  return a = Ion { ionNodes = [], ionVal = a, ionDefs = return () }

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
type IvoryAction = IL.Ivory IL.NoEffects

instance Show (IvoryAction a) where
  show iv = "Ivory NoEffects () [" ++ show block ++ "]"
    where (_, block) =
            ILM.runIvory $ ILM.noReturn $ ILM.noBreak $ ILM.noAlloc iv

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

-- | 'area', but with a 'IL.Proxy' argument to disambiguate the type.
areaP' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> IL.Proxy area -- ^ Proxy (to disambiguate type)
         -> Ion (IL.Ref IL.Global area)
areaP' name init _ = area' name init

-- | Allocate a 'IL.MemArea' for this 'Ion', returning a reference to it.
area' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> Ion (IL.Ref IL.Global area)
area' name init = Ion { ionNodes = []
                      , ionDefs = IL.defMemArea mem
                      , ionVal = IL.addrOf mem
                      }
  where mem = IL.area name init

-- | Return the Ivory 'IL.Ref' from an 'Ion' containing one (i.e. after a
-- call to 'area'' or 'areaP''
ionRef :: (IL.IvoryArea area, IL.IvoryZero area) =>
          Ion (IL.Ref IL.Global area) -> IL.Ref IL.Global area
ionRef = ionVal

-- | Specify a phase for a sub-node, returning the parent. (The sub-node may
-- readily override this phase.)
phase :: Integral i =>
         i -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase = makeSubFromAction . SetPhase Absolute Min . toInteger
-- FIXME: This needs to comprehend the different phase types.

delay :: Integral i =>
         i -- ^ Relative phase
         -> Ion a -- ^ Sub-node
         -> Ion a
delay = makeSubFromAction . SetPhase Relative Min . toInteger

-- | Specify a period for a sub-node, returning the parent. (The sub-node may
-- readily override this period.)
period :: Integral i =>
          i -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period = makeSubFromAction . SetPeriod . toInteger

-- | Combinator which simply ignores the node.  This is intended to mask off
-- some part of a spec.
disable :: Ion a -> Ion a
disable _ = Ion { ionNodes = [], ionVal = undefined, ionDefs = return () }

-- | Combinator to attach a condition to a sub-node
cond :: IvoryAction IL.IBool -> Ion a -> Ion a
cond = makeSubFromAction . AddCondition

-- | Turn an Ivory effect into an 'Ion'.
ivoryEff :: IvoryAction () -> Ion ()
ivoryEff iv = Ion { ionNodes = [defaultNode { ionAction = IvoryEff iv }]
                  , ionVal = ()
                  , ionDefs = return ()
                  }

-- | A scheduled action.  Phase and period here are absolute, and there are no
-- child nodes.
data Schedule =
  Schedule { schedId :: Integer -- ^ A unique ID for this -- action
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
modSchedule NoAction s = s
-- FIXME: Handle exact and minimum phase.

-- | Actual 'State' implementation of 'flatten'; the contained state
-- is (starting schedule state, accumulated schedule).
flattenSt :: IonNode -> State (Schedule, [Schedule]) ()
flattenSt node = do
  (ctxt, scheds) <- get
  let -- Update our context with the actions in 'node':
      ctxt' = modSchedule (ionAction node) ctxt
      ctxtNext = ctxt' { schedAction = [] }
      -- Get a unique name:
      name = schedName ctxt' ++ "_" ++ (show $ schedId ctxt')
      -- Get Ivory actions (if any) or else Nothing:
      getIvory n = case ionAction n of IvoryEff iv -> Just iv
                                       _           -> Nothing
  -- Emit schedule items for any children that have Ivory effects (We do this
  -- to combine all effects at once that are under the same parameters.)
  case (mapMaybe getIvory $ ionSub node) of
   -- For the context that we pass forward, clear out old actions:
   [] -> put (ctxt' { schedAction = [] }, scheds)
   -- If we emit a schedule item then also increment ID:
   actions -> put (ctxt' { schedAction = [], schedId = schedId ctxt + 1 }, 
                   newSched : scheds)
    where newSched = ctxt' { schedAction = actions, schedName = name }
  -- And recurse to the sub-nodes!
  mapM_ flattenSt $ ionSub node
  -- However, we must clean up by restoring whatever context we started with:
  modify $ \(c,s) -> (c { schedPath = schedPath ctxt
                        , schedName = schedName ctxt
                        }, s)
  -- FIXME: The above step seems possibly wrong.  Am I certain that no other
  -- context besides path and name requires backtracking? 
  -- FIXME: This does not handle exact or minimum phase.

-- | Walk a hierarchical 'IonNode' and turn it into a flat list of
-- scheduled actions.
flatten :: IonNode -> [Schedule]
flatten node = reverse l
  where (_, l) = execState (flattenSt node) (defaultSchedule, [])

data IonException = InvalidCName [String] String Int -- ^ Path, C name, and
                    -- index at which it is invalid
                  | PhaseExceedsPeriod [String] Integer Integer -- ^ Path,
                    -- phase, period
    deriving (Show, Typeable)

instance Exception IonException
