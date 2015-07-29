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

-- | Dummy spec for the sake of testing
test :: Ion ()
test = ion "Foo" $ do

  {-
  
  -- Period 1:
  ivoryEff $ comment "outside"

  -- This is an error, but not a very articulate one:
  --period 15 $ do
  --   ivoryEff $ comment "period 15"

  -- Period 1:
  ion "outside2" $ do
    ivoryEff $ do
      comment "also outside"

  -- This should NOT be an error:
  baz2

  period 10 $ phase 5 $ ion "Quux" $ do
    ivoryEff $ comment "quux"

  -}

  period 20 $ phase 4 $ ion "Bar" $ do
    ivoryEff $ comment "foo"
    ivoryEff $ comment "bar"
    phase 6 $ ion "phase 6" $ do
      ivoryEff $ comment "phase 6 comment"

-- test_ = execState test defaultNode
