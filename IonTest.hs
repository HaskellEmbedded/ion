{- |
Module: IonTest
Description: Example Ion module & code generation
Copyright: (c) 2015 Chris Hodapp
-}
module IonTest where

import           Control.Exception

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
  catch
    (runCompiler [ionModule test] [] ivoryOpts)
    $ \e -> putStrLn ("Exception: " ++ show (e :: IonException))

baz :: Ion ()
baz = ion "Baz" $ phase 10 $ do
  ivoryEff $ comment "probably erased"
  phase 20 $ ivoryEff $ comment "probably overridden"

baz2 :: Ion ()
baz2 = phase 10 $ ion "Baz" $ do
  ivoryEff $ comment "probably not erased"

test3 = ((ivoryEff $ comment "foo") >> (ivoryEff $ comment "foo") >> (period 20 $ ivoryEff $ comment "bar") >> (period 30 $ period 25 $ ivoryEff $ comment "bar") >> (phase 10 $ ivoryEff $ comment "fooo") >> (ion "Test" $ ivoryEff $ comment "baaar"))

-- | Dummy spec for the sake of testing
test :: Ion ()
test = ion "Foo" $ do

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

  ion "Baz" $ period 15 $ do
    ivoryEff $ comment "Foo.Baz period 15"
    ivoryEff $ comment "Foo.Baz period 15b"

  --period 20 $ do
    --ivoryEff $ comment "period 20"

  {-
  -- Period 1:
  ion "outside2" $ do
    ivoryEff $ do
      comment "also outside"

  -- This should NOT be an error:
  baz2

  period 10 $ phase 5 $ ion "Quux" $ do
    ivoryEff $ comment "quux"

  period 20 $ phase 4 $ ion "Bar" $ do
    ivoryEff $ comment "foo"
    ivoryEff $ comment "bar"
    phase 6 $ ion "phase 6" $ do
      ivoryEff $ comment "phase 6 comment"
  -}

-- test_ = execState test defaultNode
