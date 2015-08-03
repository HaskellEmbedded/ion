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
      (entry, ionDef') = ionDef "test_ion" test
      ionModule = package "ion" $ ionDef'
  catch
    (runCompiler [ionModule] [] ivoryOpts)
    $ \e -> putStrLn ("Exception: " ++ show (e :: IonException))

baz :: Ion ()
baz = ion "extBaz1" $ phase 10 $ do
  ivoryEff $ comment "should be phase 10"
  phase 20 $ ivoryEff $ comment "should be phase 20"

baz2 :: Ion ()
baz2 = phase 10 $ ion "extBaz2" $ do
  ivoryEff $ comment "should be phase 10"

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

  ion "Baz" $ period 1500 $ do
    ivoryEff $ comment "Foo.Baz period 15"
    ivoryEff $ comment "Foo.Baz period 15b"

  baz
  baz2
