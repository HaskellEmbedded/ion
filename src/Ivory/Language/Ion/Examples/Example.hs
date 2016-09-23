{- |
Module: Example
Description: Example Ion modules & code generation
Copyright: (c) 2015 Chris Hodapp

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Data.Word

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
  ionCompile ivoryOpts "simpleSchedule" simpleSchedule
  ionCompile ivoryOpts "timer" exampleTimer
  ionCompile ivoryOpts "exampleChain" exampleChain
  ionCompile ivoryOpts "giant_ugly_test" test
  return ()

printf :: Def ('[IString] :-> Sint32)
printf = importProc "printf" "stdio.h"

-- void foo(int16_t)
foo :: Def ('[] :-> ())
foo = importProc "foo" "something.h"

-- void bar(int32_t)
bar :: Def ('[] :-> ())
bar = importProc "bar" "something.h"

-- uint16_t get_value(int32_t)
get_value :: Def ('[] :-> Uint16)
get_value = importProc "get_value" "something.h"

-- bool get_flag(void)
get_flag :: Def ('[] :-> IBool)
get_flag = importProc "get_flag" "something.h"

simpleSchedule :: Ion ()
simpleSchedule = ion "schedule" $ do
  
  period 100 $ do
    variousPhases

  cond ((>? 10) <$> call get_value) $ do
    ivoryEff $ comment "get_value() > 10"
    cond (call get_flag) $ do
      ivoryEff $ comment "get_value() > 10 && get_flag()"

variousPhases :: Ion ()
variousPhases = do
    phase 1 $ ivoryEff $ do
      comment "period 100, phase 1"
      call_ foo
    phase 10 $ ion "optional_tag" $ ivoryEff $ do
      comment "period 100, phase 10"
      call_ bar
    disable $ phase 20 $ ivoryEff $ do
      comment "shouldn't even appear in code"
      call_ foo
      call_ bar
    delay 50 $ do
      p <- getSched
      ivoryEff $ do
        comment "Should be phase 100 + 50"
        comment ("Reported sched: " ++ show p)
      delay 10 $ ion "moreDelay" $ do
        p <- getSched
        ivoryEff $ do
          comment "Should be phase 100 + 50 + 10"
          comment ("Reported sched: " ++ show p)
      phase 1 $ do
        ivoryEff $ comment "Should override to phase 1"
    period 1000 $ do
      ivoryEff $ comment "Should override all other period"

-- This returns its own entry procedure (init).  The schedule procedure
-- must be called at regular intervals for the timer to function.
exampleTimer :: Ion (Def ('[] ':-> ()))
exampleTimer = ion "timer" $ mdo
  -- Note the use of 'mdo' so that we can define things in a more
  -- logical order.
  
  -- Timer is initialized with a Uint16; procedure called at
  -- expiration is fixed at compile-time:
  timer1 <- period 1 $ timer (Proxy :: Proxy Uint16) expire

  -- Initialization procedure:
  init <- newProc $ body $ do
    -- Trigger the timer for 1000 ticks:
    startTimer timer1 1000
  
  expire <- newProc $ body $ do
    call_ printf "Timer expired!\r\n"

  return init

-- | This is an example of chaining together a variety of calls and
-- async callbacks in continuation-passing style.
exampleChain :: Ion (Def ('[] ':-> ()))
exampleChain = mdo
  let error :: Def ('[Uint32] :-> ())
      error = importProc "assert_error" "foo.h"

  -- Chain together four calls with different values.  The final
  -- call is the 'success' function.
  init <- exampleSend 0x1234 error =<<
          adapt_0_1 =<< exampleSend 0x2345 error =<<
          adapt_0_1 =<< exampleSend 0x3456 error =<<
          adapt_0_1 =<< exampleSend 0x4567 error success
  -- adapt_0_1 is required to match the success callback (which takes a
  -- single Uint16) with the entry function of 'exampleSend' (which takes no
  -- arguments).

  success <- newProc $ \_ -> body $ do
    call_ printf "All calls succeeded!\r\n"

  return init

-- | This definition accepts a payload to transmit, an error callback,
-- and a success callback; it returns the entry function which
-- transmits that value, awaits the async call, and if the result is
-- correct, calls the success callback.  If any of these steps go
-- wrong, it calls the error handler with an error code, and proceeds
-- no further.
exampleSend :: Word16 -- ^ Payload value (or something like that)
               -> Def ('[Uint32] ':-> ()) -- ^ Error callback
               -> Def ('[Uint16] ':-> ()) -- ^ Success callback
               -> Ion (Def ('[] ':-> ()))
exampleSend payload err succ = mdo
  -- Make up a hypothetical function which takes a Uint16 payload to
  -- transmit, and a function pointer to a callback.  It returns a
  -- Uint32 that is an error code.  The function pointer itself takes
  -- a Uint16 which is the value received, and returns nothing.
  let transmit_async :: Def ('[Uint16, ProcPtr ('[Uint16] :-> ())] :-> Uint32)
      transmit_async = importProc "transmit_async" "foo.h"

  write <- newProc $ body $ do
    comment $ "Transmit value: " ++ show payload
    -- Tell transmit_async to transmit this, and call us back at 'recv'
    -- (which we define after):
    errCode <- call transmit_async (fromIntegral payload) $ procPtr recv
    -- Check for a nonzero error code:
    ifte_ (errCode /=? 0)
      (call_ err errCode)
      $ return ()

  recv <- newProc $ \value -> body $ do
    -- Say that hypothetically we should have received the same value
    -- back, so check this first:
    ifte_ (value /=? fromIntegral payload)
      -- If a mismatch, then call the error handler with some code:
      (call_ err 0x12345678)
      -- Otherwise, call the success handler:
      $ call_ succ value

  return write

-- Problems with this spec should be fixed but it's good to have
-- around as an example:
leakageBug :: Ion ()
leakageBug = ion "leakageBug" $ do
  period 200 $ do
    expr <- newProc $ body $ retVoid
    initTimer <- period 1 $ timer (Proxy :: Proxy Uint16) expr
    ion "otherstuff" $ ivoryEff $ do
      comment "Should be period 200 (inherited)"

-- Likewise, problems with this spec should be fixed but it's good to
-- have around as an example:
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

-- | The below does nothing useful, but is left here because it served
-- to illuminate many pesky bugs in Ion.
test :: Ion ()
test = ion "Foo" $ do

  test <- areaP' (Proxy :: Proxy (Stored Uint16)) "testMem" Nothing

  leakageBug

  lostAttribBug

  period 20 $ do
    ivoryEff $ comment "period 20a"
    ivoryEff $ comment "period 20b"
    ivoryEff $ comment "period 20c"
    ivoryEff $ comment "period 20d"
    period 30 $ ivoryEff $ comment "period 30 overwriting 20"

  period 1 $ disable $ do
    ivoryEff $ comment "shouldn't appear in code"
    period 30 $ ivoryEff $ comment "also shouldn't appear in code"
    undefined

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
