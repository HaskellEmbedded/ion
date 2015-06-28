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

   * I can do a relative phase; what about a relative period? That is, a
period which is relative not to the base rate, but to the last rate that was
inherited.
   * Use Control.Monad.Except or ExceptT to get error-handling in this.
   * The counterpart to 'cond' in Atom should compose as 'phase' and 'period'
do.
   * Figure out how to get Ivory effects into here (and what type of effects
they should be).

-}
{-# LANGUAGE FlexibleInstances #-}

module Ion where

import           Control.Monad
import           Control.Monad.State.Lazy

import           Ivory.Language

-- | The monad for expressing an Ion specification.
type Ion a = State IonNode a

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
  , ionAction :: [String] -- ^ Actions to run (this is a dummy type for
    -- debugging now)
  , ionEff :: Ivory NoEffects () -- ^ The Ivory effect that this node should
    -- perform. Note that this purposely forbids breaking, returning, and
    -- allocating.
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
                      , ionAction = []
                      , ionEff = return ()
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
ion name ion0 = do
  s <- get
  -- Run 'ion0' starting from our current state, but with 'ionSub' and
  -- 'ionAction' cleared (those apply only for the immediate state), a new
  -- 'ionName', and incremented 'ionPath':
  let (r, s') = runState ion0 $ s { ionName = name
                                  , ionPath = ionPath s ++ [name]
                                  , ionSub = []
                                  , ionAction = []
                                  , ionUnbound = False
                                  }
  -- Update our sub-ions with whatever just ran:
  put $ s { ionSub = ionSub s ++ [s']
          --, ionPhase = ionPhase s'
          --, ionPeriod = ionPeriod s'
          }
  return r

-- | Specify a phase for a sub-node. (The sub-node may readily override this
-- phase.)
phase :: Int -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase i ion0 = do
  s0 <- get
  modify $ \s -> s { ionPhase = Phase Relative Min i, ionUnbound = True }
  r <- ion0
  modify $ \s -> s { ionPhase = ionPhase s0 }
  return r
-- FIXME: This needs to comprehend the different phase types.
-- FIXME: This entire pattern can either be factored out, or possibly replaced
-- with some simpler function in Control.Monad.State.Lazy.

-- | Specify a period for a sub-node. (The sub-node may readily override this
-- period.)
period :: Int -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period i ion0 = do
  s0 <- get
  modify $ \s -> s { ionPeriod = i, ionUnbound = True }
  r <- ion0
  modify $ \s -> s { ionPeriod = ionPeriod s0 }
  return r 

-- | Dummy function for specifying an action, which right now is just a string
-- for the sake of debugging.
doThing :: String -> Ion ()
doThing str = do
  s <- get
  when (ionUnbound s) $ do
    -- TODO: Figure out why this is completely not working.
    error ("Action '" ++ str ++ " in path " ++ (show $ ionPath s) ++ ": " ++
           "This node's parameters are unbound; create a new node with 'ion'.")
  put $ s { ionAction = ionAction s ++ [str] }

ivoryEff :: Ivory NoEffects () -> Ion ()
ivoryEff iv = do
  s <- get
  put $ s { ionEff = ionEff s >> iv }
