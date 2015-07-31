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
   * I can do a relative phase; what about a relative period? That is, a
period which is relative not to the base rate, but to the last rate that was
inherited.
   * The counterpart to 'cond' in Atom should compose as 'phase' and 'period'
do.
   * A combinator to explicitly disable a rule (also composing like 'cond')
might be nice too.
   * Figure out how to get Ivory effects into here (and what type of effects
they should be).
   * Use Control.Exception for errors.
   * I need to either mandate that Ion names must be C identifiers, or make
a way to sanitize them into C identifiers.

-}
{-# LANGUAGE FlexibleInstances #-}

module Ion where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Functor
import           Data.Typeable ( Typeable )

import           Ivory.Language

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
    where ion2 = fn (ionVal ion1)

  return a = Ion { ionNodes = []
                 , ionVal = a
                 }

-- | A node representing some context in the schedule, and the actions this
-- node includes.  'ionAction' (except for 'IvoryEff' and 'NoAction') applies
-- not just to the current node, but to any child nodes too.  In general,
-- if two actions conflict (e.g. two 'SetPhase' actions with absolute phase),
-- then the innermost one overrides the other.
data IonNode = IonNode { ionAction :: IonAction -- ^ What this node does
                       , ionSub :: [IonNode] -- ^ Child nodes
                       } deriving (Show)

-- | An action/effect that a node can have.
data IonAction = IvoryEff (Ivory NoEffects ()) -- ^ The Ivory effects that this
                 -- node should perform. Note that this purposely forbids
                 -- breaking, returning, and allocating.
               | SetPhase Phase -- ^ Setting phase
               | SetPeriod Int -- ^ Setting period
               | SetName String -- ^ Setting a name
               | NoAction -- ^ Do nothing.
               deriving (Show)

instance Show (Ivory NoEffects ()) where
  show _ = "Ivory NoEffects () [no information]"

data Phase = Phase PhaseContext PhaseType Int deriving (Show)
data PhaseContext = Absolute | Relative deriving (Show)
data PhaseType = Min | Exact deriving (Show)

defaultNode = IonNode { ionAction = NoAction
                      , ionSub = []
                      }

-- | Produce a somewhat more human-readable representation of an 'IonNode'.
prettyPrint st =
  let sub s = join $ map pretty $ ionSub s
      pretty s = [ "IonNode {"
                 , " ionAction = " ++ (show $ ionAction s)
                 ] ++
                 (if null $ ionSub s
                  then []
                  else " ionSub =" : (map ("    " ++) $ sub s)) ++
                 ["}"]
  in unlines $ pretty st

makeSub :: (IonNode -> IonNode) -> Ion a -> Ion a
makeSub fn ion0 = ion0
                  { ionNodes = [(fn defaultNode) { ionSub = ionNodes ion0 }] }

-- | Create a new node with the given one as a sub-node, setting the given
-- action on this new node.
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
ivoryEff :: Ivory NoEffects () -> Ion ()
ivoryEff iv = Ion { ionNodes = [defaultNode { ionAction = IvoryEff iv }]
                  , ionVal = ()
                  }

-- | Given a hierarchical 'IonNode', turn it into a flat list for which
-- 'ionSub' is empty for each element, and all parameters are made absolute.
flatten :: IonNode -> [IonNode]
flatten node = fl node []
  where fl n acc = (n { ionSub = [] }) : foldr fl acc (ionSub n)

data IonException = NodeUnboundException IonNode
    deriving (Show, Typeable)

instance Exception IonException
