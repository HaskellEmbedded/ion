{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IonWriter where

import           Control.Monad.Writer

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM
import           Ivory.Language.Proc ( Def(..), Proc(..), IvoryCall_,
                                       IvoryProcDef )

-- | The type of Ivory action that an 'IonNode' can support. Note that this
-- purposely forbids breaking, returning, and allocating.
type IvoryAction = IL.Ivory IL.NoEffects

instance Show (IvoryAction a) where
  show iv = "Ivory NoEffects () [" ++ show block ++ "]"
    where (_, block) =
            ILM.runIvory $ ILM.noReturn $ ILM.noBreak $ ILM.noAlloc iv

data PhaseContext = Absolute -- ^ Phase is relative to the first tick
                             -- within a period
                  | Relative -- ^ Phase is relative to the last phase
                             -- used
                  deriving (Show)

data PhaseType = Min -- ^ Minimum phase (i.e. at this phase, or any
                     -- later point)
               | Exact -- ^ Exactly this phase
               deriving (Show)

{-
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
-}

data IonNode = IonNode { ionName :: String
                       , ionPhase :: Integer
                       , ionPeriod :: Maybe Integer
                       , ionAction :: [IvoryAction ()]
                       , ionCond :: [IvoryAction IL.IBool]
                       , ionDisable :: Bool
                       , ionChildren :: [IonNode]
                       } deriving (Show)

ionNode = IonNode { ionName = ""
                  , ionPhase = 0
                  , ionPeriod = Nothing
                  , ionAction = []
                  , ionCond = []
                  , ionDisable = False
                  , ionChildren = []
                  }

type Ion = Writer [IonNode]

subIon :: (IonNode -> IonNode) -> Ion a -> Ion a
subIon fn = mapWriter f
  where f (a, children) = (a, [node children])
        node ch = (fn ionNode) { ionChildren = ch }

mapIon :: (IonNode -> IonNode) -> Ion a -> Ion a
mapIon fn = mapWriter f
  where f (a, children) = (a, map fn' children)
        fn' node = fn node { ionChildren = map fn' $ ionChildren node }

period :: Integer -> Ion a -> Ion a
period n = mapIon $ \i ->
  i { ionPeriod = case ionPeriod i of Nothing -> Just n
                                      m@_     -> m
    }

ion :: String -> Ion a -> Ion a
ion str = subIon (\i -> i { ionName = str })

{-
mkAction :: IonAction -> Ion a -> Ion a
mkAction act i = (\a -> writer (a, [act])) =<< i

period :: Integer -> Ion a -> Ion a
period = mkAction . SetPeriod

phase :: Integer -> Ion a -> Ion a
phase = mkAction . SetPhase Absolute Exact

delay :: Integer -> Ion a -> Ion a
delay = mkAction . SetPhase Relative Exact

ion :: String -> Ion a -> Ion a
ion = mkAction . SetName

cond :: (IvoryAction IL.IBool) -> Ion a -> Ion a
cond = mkAction . AddCondition

ivoryEff :: (IvoryAction ()) -> Ion a -> Ion a
ivoryEff = mkAction . IvoryEff
-}

test :: Ion ()
test = do
  ion "foo" $ period 20 $ ion "bar" $ do
    ion "baz" $ return ()
    ion "quux" $ return ()
    period 10 $ ion "period10" $ period 5 $ return ()
    period 10 $ ion "period10b" $ return ()
    return ()
  period 40 $ do
    return ()
  return ()
