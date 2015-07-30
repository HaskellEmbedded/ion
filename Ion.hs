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
data Ion a = Ion { ionNode :: IonNode -- ^ The current node
                 , ionAccum :: [IonNode] -- ^ Accumulated 'old' nodes
                 , ionParent :: IonNode -- ^ The inherited context
                 , ionVal :: a
                 } deriving (Show)

ionNodes :: Ion a -> [IonNode]
ionNodes i = ionNode i : ionAccum i

instance Functor Ion where
  fmap f ion = ion { ionVal = f $ ionVal ion }

instance Applicative Ion where
  pure = return
  (<*>) = ap {-Ion { ionNode = ionNode ionA
                       , ionAccum = ionNode ionFn :
                                    (ionAccum ionFn ++ ionAccum ionA)
                       , ionParent = ionParent ionA
                       , ionVal = (ionVal ionFn) (ionVal ionA)
                       }-}

instance Monad Ion where

  ion1 >>= fn = Ion { ionNode = parent
                    , ionAccum = node2' : ionAccum ion1
                    , ionParent = parent
                    , ionVal = ionVal ion2
                    }
    where ion2 = fn (ionVal ion1)
          node1 = ionNode ion1
          node2 = (ionNode ion2)
          node2' = node2 { ionAction = ionAction node1 ++ ionAction node2 }
          parent = ionParent ion1

  return a = Ion { ionNode = defaultNode
                 , ionAccum = []
                 , ionParent = defaultNode
                 , ionVal = a
                 }

-- | A node representing some context in the schedule, and the actions this
-- node includes.
data IonNode =
  IonNode
  { ionName :: String -- ^ Name of this node
  , ionPath :: [String] -- ^ The path to this node (as a list of names,
    -- top-level first)
  , ionPeriod :: Int -- ^ The current period of the base rate
  , ionPhase :: Phase -- ^ The current phase within that period
  , ionAction :: [Ivory NoEffects ()] -- ^ The Ivory effects that this node
    -- should perform. Note that this purposely forbids breaking, returning,
    -- and allocating.
  } deriving (Show)

instance Show (Ivory NoEffects ()) where
  show _ = "Ivory NoEffects () [no information]"

data Phase = Phase PhaseContext PhaseType Int deriving (Show)
data PhaseContext = Absolute | Relative deriving (Show)
data PhaseType = Min | Exact deriving (Show)

defaultNode = IonNode { ionPeriod = 1
                      , ionName = "root"
                      , ionPhase = Phase Absolute Min 0
                      , ionPath = [ionName defaultNode]
                      , ionAction = []
                      }

-- | Produce a somewhat more human-readable representation of an 'IonNode'.
prettyPrint :: IonNode -> String
prettyPrint st = unlines $ pretty st
  where --sub s = join $ map pretty $ ionSub s
    pretty s = [ "IonNode {"
               , " ionName = " ++ (show $ ionName s)
               , " ionPath = " ++ (show $ ionPath s)
               , " ionPeriod = " ++ (show $ ionPeriod s)
               , " ionPhase = " ++ (show $ ionPhase s)
               , " ionAction = " ++ (show $ ionAction s)
               , "}"]

-- | Create a new named sub-node.
ion :: String -- ^ Name
       -> Ion a -- ^ Sub-node
       -> Ion a
ion name ion_ = ion_ { ionNode = node'
                     , ionParent = node
                     }
  where node = ionNode ion_
        node' = node { ionName = name
                     , ionPath = name : (ionPath node)
                     }

-- | Specify a phase for a sub-node. (The sub-node may readily override this
-- phase.)
phase :: Int -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase i ion_ = ion_ { ionNode = node { ionPhase = Phase Relative Min i }
                    , ionParent = node
                    }
  where node = ionNode ion_
-- FIXME: This needs to comprehend the different phase types.

-- | Specify a period for a sub-node. (The sub-node may readily override this
-- period.)
period :: Int -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period i ion_ = ion_ { ionNode = node'
                     , ionParent = node
                     }
  where node = ionNode ion_
        node' = node { ionPeriod = i
                     }

-- | Add an Ivory action to this node. (I should probably give this a better
-- name at some point, and maybe move it into IonIvory.)
ivoryEff :: Ivory NoEffects () -> Ion ()
ivoryEff iv = Ion { ionNode = defaultNode { ionAction = [iv] }
                  , ionAccum = []
                  , ionVal = ()
                  , ionParent = defaultNode
                  }

-- | Given a hierarchical 'IonNode', turn it into a flat list for which
-- 'ionSub' is empty for each element, and all parameters are made absolute.
{-
flatten :: IonNode -> [IonNode]
flatten node = fl node []
  where fl n acc = (n { ionSub = [] }) : foldr fl acc (ionSub n)
-}

data IonException = NodeUnboundException IonNode
    deriving (Show, Typeable)

instance Exception IonException
