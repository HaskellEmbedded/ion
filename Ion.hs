{- |
Module: Ion
Description: Top-level Ion module
Copyright: (c) 2015 Chris Hodapp

Ion is a Haskell EDSL that is inspired by another EDSL,
<https://hackage.haskell.org/package/atom Atom>.  Ion aims to be a
re-implementation of Atom which, rather than generating C code directly (as
Atom does), interfaces with another very powerful, more general EDSL,
<http://ivorylang.org/ Ivory>.

-}

module Ion where

import           Control.Monad
import           Control.Monad.State.Lazy

-- | The monad for expressing an Ion specification.
type Ion a = State IonNode a

-- | A node representing some context in the schedule (including its path and
-- sub-nodes), and the actions this node includes.
data IonNode = IonNode
               { ionName :: String -- ^ Name of this node
               , ionPath :: [String] -- ^ The path to this node (as a list of
                            -- names, top-level first)
               , ionSub :: [IonNode] -- ^ Sub-nodes we've accumulated
               , ionPeriod :: Int -- ^ The current period of the base rate
               , ionPhase :: Phase -- ^ The current phase within that period
               , ionAction :: [String] -- ^ Actions to run (this is a dummy
                              -- type for debugging now)
               } deriving (Show)

-- Speculative ideas:
-- - I can do a relative phase; what about a relative period? That is, a period
-- which is relative not to the base rate, but to the last rate that was
-- inherited?

data Phase = Phase PhaseContext PhaseType Int deriving (Show)
data PhaseContext = Absolute | Relative deriving (Show)
data PhaseType = Min | Exact deriving (Show)

defaultNode = IonNode { ionPeriod = 1
                      , ionName = "root"
                      , ionPhase = Phase Absolute Min 1
                      , ionPath = [ionName defaultNode]
                      , ionSub = []
                      , ionAction = []
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
                                  }
  -- Update our sub-ions with whatever just ran:
  put $ s { ionSub = ionSub s ++ [s'] }
  return r

-- | Specify a phase for a sub-node. (The sub-node may readily override this
-- phase.)
phase :: Int -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase i ion0 = do
  s <- get
  let (r, s') = runState ion0 $ s { ionAction = []
                                  , ionPhase = Phase Absolute Min i
                                  }
  put s'
  return r
-- FIXME: This needs to comprehend the different phase types.
-- FIXME: This very likely can be written very easily with mapState too.

-- | Specify a period for a sub-node. (The sub-node may readily override this
-- period.)
period :: Int -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period i ion0 = do
  s <- get
  let (r, s') = runState ion0 $ s { ionAction = []
                                  , ionPeriod = i
                                  }
  put s'
  return r
-- FIXME: This very likely can be written very easily with mapState, e.g.
-- mapState $ \(a,s) -> (a, s { ionAction = [], ionPeriod = i })
-- but the above has some bugs.

-- | Dummy function for specifying an action, which right now is just a string
-- for the sake of debugging.
doThing :: String -> Ion ()
doThing str = do
  s <- get
  put $ s { ionAction = ionAction s ++ [str] }

-- | Dummy spec for the sake of testing
test :: Ion ()
test = ion "Foo" $ do

  doThing "outside"
  
  ion "Bar" $ do
    doThing "foo"
    doThing "bar"
    ion "Another" $ do
      doThing "other_bar"

  ion "Baz" $ do
    doThing "baz"

  period 10 $ phase 5 $ ion "Quux" $ do
    doThing "quux"

s = execState test defaultNode
