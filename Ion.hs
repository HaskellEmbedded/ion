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
   * Counterpart to 'cond' in Atom should compose as 'phase' and 'period' do.
   * A combinator to explicitly disable a rule (also composing like 'cond')
might be nice too.
   * I need to either mandate that Ion names must be C identifiers, or make
a way to sanitize them into C identifiers.

-}
{-# LANGUAGE FlexibleInstances #-}

module Ion where

import           Control.Exception
import           Control.Monad
import           Data.Maybe ( mapMaybe )

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM

-- | The monad for expressing an Ion specification.
data Ion a = Ion { ionNodes :: [IonNode]
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
-- FIXME: Can we show anything useful about an Ivory effect?  Ivory can show
-- ASTs.

-- | An action/effect that a node can have.
data IonAction = IvoryEff IvoryAction -- ^ The Ivory effects that this
                 -- node should perform
               | SetPhase Phase -- ^ Setting phase
               | SetPeriod Int -- ^ Setting period
               | SetName String -- ^ Setting a name
               | NoAction -- ^ Do nothing.
               deriving (Show)

data Phase = Phase PhaseContext PhaseType Int deriving (Show)
data PhaseContext = Absolute | Relative deriving (Show)
data PhaseType = Min | Exact deriving (Show)

defaultNode = IonNode { ionAction = NoAction
                      , ionSub = []
                      }

-- | Produce a somewhat more human-readable representation of an 'Ion'.
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

-- | Create a new node with the given one as a sub-node, using the given
-- function to transform the node.  (FIXME: Terminology is kind of hairy. 'Ion'
-- is not 'IonNode'.)
makeSub :: (IonNode -> IonNode) -> Ion a -> Ion a
makeSub fn ion0 = ion0
                  { ionNodes = [(fn defaultNode) { ionSub = ionNodes ion0 }] }

-- | Create a new node with the given one as a sub-node, setting the given
-- action on this new node.  (FIXME: Terminology is kind of hairy. 'Ion' is
-- not 'IonNode'.)
makeSubFromAction :: IonAction -> Ion a -> Ion a
makeSubFromAction act = makeSub (\i -> i { ionAction = act })

-- | Create a new named sub-node.
ion :: String -- ^ Name
       -> Ion a -- ^ Sub-node
       -> Ion a
ion = makeSubFromAction . SetName

-- | Specify a phase for a sub-node. (The sub-node may readily override this
-- phase.)
phase :: Int -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase = makeSubFromAction . SetPhase . Phase Relative Min
-- FIXME: This needs to comprehend the different phase types.

-- | Specify a period for a sub-node. (The sub-node may readily override this
-- period.)
period :: Int -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period = makeSubFromAction . SetPeriod

-- | Add an Ivory action to this node. (I should probably give this a better
-- name at some point, and maybe move it into IonIvory.)
ivoryEff :: IL.Ivory IL.NoEffects () -> Ion ()
ivoryEff iv = Ion { ionNodes = [defaultNode { ionAction = IvoryEff iv }]
                  , ionVal = ()
                  }

-- | Scheduled action, derived loosely from 'IonNode'.  Phase and period here
-- are absolute.
data Schedule = Schedule { schedName :: String
                         , schedPath :: [String]
                         , schedPhase :: Int
                         , schedPeriod :: Int
                         , schedAction :: [IvoryAction]
                         }
              deriving (Show)

defaultSchedule = Schedule { schedName = "root"
                           , schedPath = []
                           , schedPhase = 0
                           , schedPeriod = 1
                           , schedAction = []
                           }

-- | Walk a hierarchical 'IonNode' and turn it into a flat list of
-- scheduled actions, given a starting context (another 'Schedule')
flatten :: Schedule -> IonNode -> [Schedule]
flatten ctxt node = newSched ++ (join $ map (flatten ctxtClean) $ ionSub node)
  where ctxt' = case ionAction node of
                 IvoryEff iv -> ctxt
                 SetPhase (Phase _ _ ph) -> ctxt { schedPhase = ph }
                                            -- FIXME: Handle real phase.
                 SetPeriod p -> ctxt { schedPeriod = p }
                 SetName name -> ctxt { schedName = name
                                      , schedPath = name : schedPath ctxt
                                      }
                 NoAction -> ctxt
                 a@_ -> error ("Unknown action type: " ++ show a)
        -- For the context that we pass forward, clear out the action; actions
        -- run only once:
        ctxtClean = ctxt' { schedAction = [] }
        -- Emit schedule nodes for any children that have Ivory effects:
        -- (We do this to combine all effects at once that are under the same
        -- parameters.)
        getIvory node = case ionAction node of IvoryEff iv -> Just iv
                                               _           -> Nothing
        ivoryActions = mapMaybe getIvory $ ionSub node
        newSched = if null ivoryActions
                   then []
                   else [ctxt' { schedAction = ivoryActions }]
-- FIXME: When we have adjacent IvoryEff actions, combine these into a single
-- schedAction.

data IonException = NodeUnboundException IonNode
    deriving (Show)

instance Exception IonException
