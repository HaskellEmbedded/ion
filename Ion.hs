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

   * I need to convert over the 'schedule' function in Schedule.hs in Atom.
   * I can do a relative phase; what about a relative period? That is, a
period which is relative not to the base rate, but to the last rate that was
inherited.
   * Use Control.Monad.Except, ExceptT, or MonadError to get error-handling in
this. (I think HStringTemplate in Ivory is pinned to version 0.8.3 which
restricts us from using mtl 2.2.1 which has ExceptT.  However, MonadError is
still present, as is ErrorT, albeit deprecated.)
   * The counterpart to 'cond' in Atom should compose as 'phase' and 'period'
do.
   * A combinator to explicitly disable a rule (also composing like 'cond')
might be nice too.
   * Figure out how to get Ivory effects into here (and what type of effects
they should be).

-}
{-# LANGUAGE FlexibleInstances #-}

module Ion where

import           Control.Monad
--import           Control.Monad.Except
import           Control.Monad.Error
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Class

import           Ivory.Language

-- | The monad for expressing an Ion specification.
type IonM = State IonNode
type Ion = ErrorT String IonM

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
  , ionAction :: Ivory NoEffects () -- ^ The Ivory effect that this node should
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
                      , ionAction = return ()
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
{-ion_ :: String -- ^ Name
       -> Ion a -- ^ Sub-node
       -> Ion a-}
--ion_ :: Monad m => String -> m (IonM a) -> m (IonM a)
ion_ :: String -> ErrorT String IonM a -> ErrorT String IonM a
ion_ name ion0 = liftM (ion name) ion0
-- Why does the above type sig fail to work?

-- | Create a new named sub-node.
ion :: String -- ^ Name
       -> IonM a -- ^ Sub-node
       -> IonM a
ion name ion0 = do
  s <- get
  -- Run 'ion0' starting from our current state, but with 'ionSub' and
  -- 'ionAction' cleared (those apply only for the immediate state), a new
  -- 'ionName', and incremented 'ionPath':
  let (r, s') = runState ion0 $ s { ionName = name
                                  , ionPath = ionPath s ++ [name]
                                  , ionSub = []
                                  , ionAction = return ()
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

-- | Add an Ivory action to this node. (I should probably give this a better
-- name at some point.)
ivoryEff :: Ivory NoEffects () -> Ion ()
ivoryEff iv = do
  s <- get
  when (ionUnbound s) $ do
    -- TODO: Figure out why this is completely not working.
    error ("Action in path " ++ (show $ ionPath s) ++ ": " ++
           "This node's parameters are unbound; create a new node with 'ion'.")
  put $ s { ionAction = ionAction s >> iv }
