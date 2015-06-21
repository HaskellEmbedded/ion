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
phase i = withState $ \s -> s { ionAction = []
                              , ionPhase = Phase Absolute Min i
                              }

-- | Specify a period for a sub-node. (The sub-node may readily override this
-- period.)
period :: Int -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period i = withState $ \s -> s { ionAction = [], ionPeriod = i }

-- | Dummy function for specifying an action, which right now is just a string
-- for the sake of debugging.
doThing :: String -> Ion ()
doThing str = do s <- get
                 put $ s { ionAction = ionAction s ++ [str] }

-- | Dummy spec for the sake of testing
test :: Ion ()
test = ion "Foo" $ do

  doThing "outside"
  
  period 20 $ phase 4 $ ion "Bar" $ do
    -- FIXME: There is still an issue in which constructs like the above (which
    -- work fine in Atom) have their period and phase 'escape' their scope and
    -- apply to sub-nodes that are below.
    -- Since 'ion' just inherits its starting context, and returns its starting
    -- context (intentionally), I'm not sure of a quick way around this.
    -- My expectation is that "phase N f" has an effect *only* inside 'f', and
    -- nowhere past.
    doThing "foo"
    doThing "bar"
    ion "Another" $ phase do
      doThing "other_bar"

  ion "Baz" $ do
    doThing "baz"

  period 10 $ phase 5 $ ion "Quux" $ do
    doThing "quux"

s = execState test defaultNode
