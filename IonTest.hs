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
  doThing "probably erased"
  phase 20 $ doThing "probably overridden"

baz2 :: Ion ()
baz2 = phase 10 $ ion "Baz" $ do
  doThing "probably not erased"

-- | Dummy spec for the sake of testing
test :: Ion ()
test = ion "Foo" $ do

  -- Period 1:
  doThing "outside"

  -- This gets period 1:
  period 15 $ do
    doThing "period 15"

  -- Period 1:
  ion "outside2" $ do
    doThing "also outside"
    ivoryEff $
      comment "Woo!"

  baz

  baz2

  period 10 $ phase 5 $ ion "Quux" $ do
    doThing "quux"
  
  period 20 $ phase 4 $ ion "Bar" $ do
    doThing "foo"
    doThing "bar"
    ion "Another" $ phase 7 $ do
      doThing "other_bar"
      -- FIXME: The above doThing is getting completely lost.
      -- FIXME: So is the 'phase 7' and 'phase 8' below...
    ion "Yet another" $ phase 8 $ do
      doThing "yet_another_bar"
      -- So is this.

test_ = execState test defaultNode

