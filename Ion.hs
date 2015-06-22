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
  -- Acquire the starting state:
  start <- get
  -- Run 'ion0' from there:
  let ph = Phase Relative Min i
      (r, s') = runState ion0 $ start { ionAction = []
                                      , ionSub = []
                                      , ionPhase = ph
                                      }
  -- Update our starting state with any sub-nodes:
  put $ start { ionSub = ionSub start ++ ionSub s' }
  -- Pass along the value that ion0 produced:
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
  -- Acquire the starting state:
  start <- get
  -- Run 'ion0' from there:
  let (r, s') = runState ion0 $ start { ionAction = []
                                      , ionSub = []
                                      , ionPeriod = i
                                      }
  -- Update our starting state with any sub-nodes:
  put $ start { ionSub = ionSub start ++ ionSub s' }
  -- Pass along the value that ion0 produced:
  return r

-- | Dummy function for specifying an action, which right now is just a string
-- for the sake of debugging.
doThing :: String -> Ion ()
doThing str = do s <- get
                 put $ s { ionAction = ionAction s ++ [str] }

baz :: Ion ()
baz = ion "Baz" $ phase 10 $ do
  doThing "probably erased"

baz2 :: Ion ()
baz2 = phase 10 $ ion "Baz" $ do
  doThing "probably not erased"

-- | Dummy spec for the sake of testing
test :: Ion ()
test = ion "Foo" $ do

  doThing "outside"

  baz

  baz2

  period 10 $ phase 5 $ ion "Quux" $ do
    doThing "quux"
  
  period 20 $ phase 4 $ ion "Bar" $ do
    doThing "foo"
    doThing "bar"
    ion "Another" $ phase 7 $ do
      doThing "other_bar"
      -- FIXME: The above doThing is getting completely lost.
      -- FIXME: So is the 'phase 7' and 'phase 8' below...
    ion "Yet another" $ phase 8 $ do
      doThing "yet_another_bar"
      -- So is this.

test_ = execState test defaultNode
