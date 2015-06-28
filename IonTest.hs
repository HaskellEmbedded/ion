module IonTest where

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
  runCompiler [ionModule test] [] ivoryOpts

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

  -- Period 1:
  ivoryEff $ comment "outside"

  -- This gets period 1:
  period 15 $ do
    ivoryEff $ comment "period 15"

  -- Period 1:
  ion "outside2" $ do
    ivoryEff $ do
      comment "also outside"

  baz

  baz2

  period 10 $ phase 5 $ ion "Quux" $ do
    ivoryEff $ comment "quux"
  
  period 20 $ phase 4 $ ion "Bar" $ do
    ivoryEff $ comment "foo"
    ivoryEff $ comment "bar"
    ion "Another" $ phase 7 $ do
      ivoryEff $ comment "other_bar"
      -- FIXME: The above ivoryEff $ comment is getting completely lost.
      -- FIXME: So is the 'phase 7' and 'phase 8' below...
    ion "Yet another" $ phase 8 $ do
      ivoryEff $ comment "yet_another_bar"
      -- So is this.

-- test_ = execState test defaultNode

