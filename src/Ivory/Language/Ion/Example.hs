{- |
Module: Example
Description: Example Ion modules & code generation
Copyright: (c) 2015 Chris Hodapp

-}
{-# LANGUAGE DataKinds #-}

module Ivory.Language.Ion.Example where

import           Control.Exception

import           Ivory.Language
import           Ivory.Compile.C.CmdlineFrontend

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Code
import           Ivory.Language.Ion.Operators

main :: IO ()
main = do
  let ivoryOpts = initialOpts { scErrors = False
                              , srcLocs = True
                              , outDir = Nothing
                              }
      exps = ionDef "test_ion" lostAttribBug
      mod = package "ion" $ ionModule exps
  catch
    (runCompiler [mod] [] ivoryOpts)
    $ \e -> putStrLn ("Exception: " ++ show (e :: IonException))

-- I observe problems with this spec if the 'modify' call used in
-- 'flattenSt' in Ion.hs does *not* reset schedPhase and
-- schedPeriod.
-- The bug goes away if that call does reset schedPhase and
-- schedPeriod.
leakageBug :: Ion ()
leakageBug = ion "leakageBug" $ do
  period 200 $ do
    expr <- newProc $ body $ retVoid
    initTimer <- period 1 $ timer (Proxy :: Proxy Uint16) expr
    ion "otherstuff" $ ivoryEff $ do
      comment "Should be period 200 (inherited)"

-- I observe problems with this spec if the 'modify' call used in
-- 'flattenSt' in hs *does* reset schedPhase and schedPeriod.
lostAttribBug :: Ion ()
lostAttribBug = period 200 $ ion "lostAttribBug" $ do
  phase 100 $ ion "moreStuff" $ do
    p <- getSched
    ivoryEff $ do
      comment "Phase 100"
      comment ("Reported sched: " ++ show p)
    delay 3 $ do
      p <- getSched
      ivoryEff $ do
        comment "Should be phase 103"
        comment ("Reported sched: " ++ show p)
      delay 10 $ ion "moreDelay" $ do
        p <- getSched
        ivoryEff $ do
          comment "Should be phase 113"
          comment ("Reported sched: " ++ show p)

baz :: Ion ()
baz = ion "extBaz1" $ phase 10 $ do
  ivoryEff $ comment "should be phase 10"
  phase 20 $ ivoryEff $ comment "should be phase 20"

baz2 :: Ion ()
baz2 = phase 10 $ ion "extBaz2" $ do
  ivoryEff $ comment "should be phase 10"

delayTest :: Ion ()
delayTest = ion "delayTest" $ period 100 $ do
  ivoryEff $ comment "should be phase 0"
  delay 10 $ ion "named" $ ivoryEff $ comment "delay 10 #1"
  delay 10 $ ivoryEff $ comment "delay 10 #2"
  delay 10 $ ivoryEff $ comment "delay 10 #3"
  ion "delayTest2" $ do
    delay 20 $ ivoryEff $ comment "should have inherited delay"

-- | Dummy spec for the sake of testing
test :: Ion ()
test = ion "Foo" $ do

  test <- areaP' (Proxy :: Proxy (Stored Uint16)) "testMem" Nothing
  
  period 20 $ do
    ivoryEff $ comment "period 20a"
    ivoryEff $ comment "period 20b"
    ivoryEff $ comment "period 20c"
    ivoryEff $ comment "period 20d"
    period 30 $ ivoryEff $ comment "period 30 overwriting 20"
  
  -- Period 1:
  ion "Bar" $ do
    ivoryEff $ comment "Foo.Bar"
    ivoryEff $ comment "Foo.Bar 2"

  ion "Baz" $ period 1500 $ do
    ivoryEff $ comment "Foo.Baz period 15"
    ivoryEff $ comment "Foo.Baz period 15b"

  period 75 $ do
    baz
    baz2

  -- FIXME: delayTest improperly inherits phase 10 from baz2.
  period 100 $ do
    delayTest

  disable $ ion "disabled" $ period 60000 $ do
    ivoryEff $ comment "Should be disabled"

  cond (return false) $ ion "condTest" $ do
    ivoryEff $ comment "Conditional test"
    ion "condTest1" $ ivoryEff $ comment "Conditional test sub 1"
    ion "condTest2" $ ivoryEff $ comment "Conditional test sub 2"
    ion "condTest3" $ ivoryEff $ comment "Conditional test sub 3"
    cond (return true) $ ion "twoConds" $ do
      ivoryEff $ comment "Two conditions"
      ion "condTest4" $ ivoryEff $ comment "Also two conditions"


  cond (return true) $ ion "condTest2" $ do
    ivoryEff $ comment "Should have just one condition"
