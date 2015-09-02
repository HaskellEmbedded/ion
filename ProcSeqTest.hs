{- |
Module: ProcSeqTest
Description: Example ProcSeq usage
Copyright: (c) 2015 Chris Hodapp
-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module ProcSeqTest where

import           Data.Proxy ( Proxy(..) )

import           Ivory.Language hiding ( Proxy )
import           Ivory.Compile.C.CmdlineFrontend

import           ProcSeq

main :: IO ()
main = do
  let ivoryOpts = initialOpts { scErrors = False
                              , srcLocs = True
                              , outDir = Nothing
                              }
      (entry, defs) = seqDef testSeq "test_procseq"
      ionModule = package "procseq" $ defs
  runCompiler [ionModule] [] ivoryOpts

testSeq :: ProcSeq (Def ('[] ':-> ()))
testSeq = do

  mem <- newArea $ Just $ ival (0 :: Uint16)
  
  other <- newProcP (Proxy :: Proxy (Def ('[] ':-> ()))) $ body $ do
    comment "Inside 'other'"

  start <- newProc $ body $ do
    comment "Inside 'start'"
    call_ other

  return start
