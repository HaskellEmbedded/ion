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
data Ion a = Ion IonNode a

ionNode :: Ion a -> IonNode
ionNode (Ion node _) = node

instance Functor Ion where
  fmap f (Ion n a) = Ion n (f a)

instance Applicative Ion where
  pure = return -- Ion defaultNode
  Ion n1 f <*> Ion n2 a = Ion n2 (f a) -- correct?

instance Monad Ion where

  Ion n1 f >>= k = k f
  
  return = Ion defaultNode

-- | A node representing some context in the schedule (including its path and
-- sub-nodes), and the actions this node includes.
data IonNode =
  IonNode
  { ionName :: String -- ^ Name of this node
  , ionPath :: [String] -- ^ The path to this node (as a list of names,
    -- top-level first)
  , ionSub :: [IonNode] -- ^ Sub-nodes we've accumulated
  , ionPeriod :: Int -- ^ The current period of the base rate
  , ionPhase :: Phase -- ^ The current phase within that period
  , ionAction :: Maybe (Ivory NoEffects ()) -- ^ The optional Ivory effect that
    -- this node should perform. Note that this purposely forbids breaking,
    -- returning, and allocating.
  , ionUnbound :: Bool -- ^ True if this node has parameters (particularly for
    -- the schedule context) that aren't yet 'bound' to a sub-node.
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
                      , ionSub = []
                      , ionAction = Nothing
                      , ionUnbound = False
                      }

-- | Produce a somewhat more human-readable representation of an 'IonNode'.
prettyPrint :: IonNode -> String
prettyPrint st =
  let sub s = join $ map pretty $ ionSub s
      pretty s = [ "IonNode {"
                 , " ionName = " ++ (show $ ionName s)
                 , " ionPath = " ++ (show $ ionPath s)
                 , " ionPeriod = " ++ (show $ ionPeriod s)
                 , " ionPhase = " ++ (show $ ionPhase s)
                 , " ionAction = " ++ (show $ ionAction s)
                 , " ionUnbound = " ++ (show $ ionUnbound s)
                 ] ++
                 (if null $ ionSub s
                  then []
                  else " ionSub =" : (map ("    " ++) $ sub s)) ++
                 ["}"]
  in unlines $ pretty st

-- | Create a new named sub-node.
ion :: String -- ^ Name
       -> Ion a -- ^ Sub-node
       -> Ion a
ion name (Ion node a) = Ion node' a
  where node' = defaultNode { ionName = name
                            , ionPath = ionPath node ++ [name]
                            , ionSub = [node]
                            }

-- | Specify a phase for a sub-node. (The sub-node may readily override this
-- phase.)
phase :: Int -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase i (Ion node a) = Ion node' a
  where node' = node { ionPhase = Phase Relative Min i }
-- FIXME: This needs to comprehend the different phase types.

-- | Specify a period for a sub-node. (The sub-node may readily override this
-- period.)
period :: Int -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period i (Ion node a) = Ion node' a
  where node' = node { ionPeriod = i }

-- | Add an Ivory action to this node. (I should probably give this a better
-- name at some point, and maybe move it into IonIvory.)
ivoryEff :: Ivory NoEffects () -> Ion ()
ivoryEff iv = Ion (defaultNode { ionAction = Just iv }) ()

-- | Given a hierarchical 'IonNode', turn it into a flat list for which
-- 'ionSub' is empty for each element, and all parameters are made absolute.
flatten :: IonNode -> [IonNode]
flatten node = fl node []
  where fl n acc = (n { ionSub = [] }) : foldr fl acc (ionSub n)

data IonException = NodeUnboundException IonNode
    deriving (Show, Typeable)

instance Exception IonException
