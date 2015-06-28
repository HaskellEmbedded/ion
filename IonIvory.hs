{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module IonIvory where

import           Control.Monad.State.Lazy

import           Ivory.Language

import           Ion

ionModule :: Ion () -> Module
ionModule i0 = package "ion" $ do
  incl $ ionFunction i0

ionFunction :: Ion () -> Def ('[] :-> ())
ionFunction i0 = proc "ionFunction" $ body $ do
  noReturn $ noBreak $ noAlloc $ getIvory $ execState i0 defaultNode

  -- (GetBreaks eff ~ NoBreak, GetReturn eff ~ NoReturn, GetAlloc eff ~ NoAlloc)
getIvory :: (eff ~ NoEffects) => IonNode -> Ivory eff ()
getIvory i0 = do
  -- ifte_ present only to illuminate what's what
  ifte_ true
    (do
        comment $ "name = " ++ (show $ ionName i0)
        comment $ "path = " ++ (show $ ionPath i0)
        comment $ "period = " ++ (show $ ionPeriod i0)
        comment $ "phase = " ++ (show $ ionPhase i0)
        ionAction i0
        mapM_ getIvory $ ionSub i0)
    $ return ()
