module Ion where

import           Control.Monad
import           Control.Monad.State.Lazy

-- | The scheduling state at some point
data IonState = IonState { ionPeriod :: Int
                         , ionPhase :: Phase
                         , ionPath :: [String]
                         , ionSub :: [Ion ()]
                         }

instance Show IonState where
  show st =
    "IonState {" ++
    "ionPeriod = " ++ (show $ ionPeriod st) ++
    ", ionPhase = " ++ (show $ ionPhase st) ++
    ", ionPath = " ++ (show $ ionPath st) ++
    ", ionSub = " ++
    (show $ map (\s -> snd $ runState s defaultIonState) $ ionSub st) ++ "}"

defaultIonState = IonState { ionPeriod = 1
                           , ionPhase = Phase Absolute Min 1
                           , ionPath = []
                           , ionSub = []
                           }

data Phase = Phase PhaseContext PhaseType Int deriving (Show)
data PhaseContext = Absolute | Relative deriving (Show)
data PhaseType = Min | Exact deriving (Show)

type Ion a = State IonState a

ion :: String -> Ion () -> Ion ()
ion name ion' = do s <- get
                   put $ s { ionPath = name : (ionPath s)
                           , ionSub = ion' : (ionSub s)
                           }

phase :: Int -> Ion a -> Ion a
phase i ion' = do
  s <- get
  r <- ion'
  put $ s { ionPhase = Phase Absolute Min i }
  return r

period :: Int -> Ion a -> Ion a
period i ion' = do
  s <- get
  r <- ion'
  put $ s { ionPeriod = i }
  return r

test :: Ion ()
test = ion "Foo" $ do

  phase 15 $ ion "Bar" $ do
    return ()

  period 50 $ ion "Baz" $ do
    period 100 $ ion "Quux" $ do
      return ()
    return ()

  return ()

(_, s) = runState test defaultIonState

