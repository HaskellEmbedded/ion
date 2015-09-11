{- |
Module: IonSeqTest
Description: Example IonSeq usage
Copyright: (c) 2015 Chris Hodapp
-}
{-# LANGUAGE DataKinds, RecursiveDo, TypeOperators #-}
module IonSeqTest where

import           Ivory.Language
import           Ivory.Compile.C.CmdlineFrontend

import           Ion
import           IonIvory

main :: IO ()
main = do
  let ivoryOpts = initialOpts { scErrors = False
                              , srcLocs = True
                              , outDir = Nothing
                              }
      exp = ionDef "test_procseq" testSeq
      ionMod = package "procseq" $ ionModule exp
  runCompiler [ionMod] [] ivoryOpts

{-
main2 :: IO ()
main2 = do
  let ivoryOpts = initialOpts { scErrors = False
                              , srcLocs = True
                              , outDir = Nothing
                              }
      (entry, seqDef') = seqDef testSeq "test_timer"
      (timeFn, ionDef') = ionDef "test_ion" test
      ionModule = package "procseq2" $ seqDef' >> ionDef'
  runCompiler [ionModule] [] ivoryOpts
-}

-- Note the use of 'RecursiveDo' and 'mdo' to allow us to write things in a
-- more logical sequence.
testSeq :: IonSeq (Def ('[] ':-> ()))
testSeq = mdo

  mem <- newArea $ Just $ ival (0 :: Uint16)
  
  start <- newProc $ body $ do
    comment "Inside 'start'"
    call_ other

  t <- timer (Proxy :: Proxy Uint16) timerCb

  other <- newProcP (Proxy :: Proxy (Def ('[] ':-> ()))) $ body $ do
    comment "Inside 'other'"
    startTimer t 5000

  timerCb <- newProc $ body $ do
    comment "Timer returned"

  return start

{-
testTimer :: IonSeq (Ion (Ref Global (Stored Uint16)))
testTimer = mdo

  let t' = timer (Proxy :: Proxy Uint16) timerCb
  t <- t'

  other <- newProcP (Proxy :: Proxy (Def ('[] ':-> ()))) $ body $ do
    comment "Inside 'other'"
    startTimer t 5000

  timerCb <- newProc $ body $ do
    comment "Timer returned"

  ion_ "disabled" $ period 60000 $ do
    ivoryEff $ comment "Inside IonSeq"

  return t
-}
