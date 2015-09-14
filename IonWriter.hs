{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IonWriter where

import           Control.Monad.Writer
-- import           Data.Tree

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
               | Disable -- ^ Disable this node and all children
               | NoAction -- ^ Do nothing
               deriving (Show)

data IonNode = IonNode { actions :: IonAction
                       , children :: [IonNode]
                       } deriving (Show)

type Ion = Writer [IonNode]

addAction :: IonAction -> Ion a -> Ion a
addAction act = mapWriter f
  where f (a, nodes) = (a, [IonNode { actions = act, children = nodes }])

period :: Integer -> Ion a -> Ion a
period = addAction . SetPeriod

phase :: Integer -> Ion a -> Ion a
phase = addAction . SetPhase Absolute Exact

delay :: Integer -> Ion a -> Ion a
delay = addAction . SetPhase Relative Exact

ion :: String -> Ion a -> Ion a
ion = addAction . SetName

cond :: (IvoryAction IL.IBool) -> Ion a -> Ion a
cond = addAction . AddCondition

ivoryEff :: (IvoryAction ()) -> Ion a -> Ion a
ivoryEff = addAction . IvoryEff

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
