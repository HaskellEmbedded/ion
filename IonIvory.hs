{- |
Module: IonIvory
Description: Conversion from Ion to Ivory modules & procedures
Copyright: (c) 2015 Chris Hodapp

This contains functionality for converting the 'Ion' type to Ivory constructs.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module IonIvory where

import           Control.Exception

import           Ivory.Language
import           Ivory.Language.MemArea ( memSym )
import           Ivory.Language.Monad ( emit )
import qualified Ivory.Language.Syntax.AST as AST
import qualified Ivory.Language.Syntax.Names as N
import qualified Ivory.Language.Syntax.Type as Ty

import           Ion
import           IonUtil

-- | Generate an Ivory module from the given Ion spec.
ionModule :: Ion () -> Module
ionModule i0 = package "ion" $ do
  let nodes = flatten defaultSchedule $ head $ ionNodes i0
  sequence_ $ ionProc nodes

-- | Generate Ivory procedures for the given Ion spec.
-- FIXME: This needs to expose the entry procedure or Ivory can't call it
ionProc :: [Schedule] -> [ModuleDef]
ionProc scheds = (map incl $ entryProc : schedFns) ++
                 (map (defMemArea . counter) scheds)
  where schedFns :: [Def ('[] ':-> ())]
        schedFns = map mkSchedFn scheds
        counter sch = area name $ Just $ ival (start :: Uint16)
          where start = fromIntegral $ schedPhase sch
                name = "counter_" ++ schedName sch
        mkSchedFn sch = proc ("ion_" ++ schedName sch) $ body $ do
          noReturn $ noBreak $ noAlloc $ getIvory sch
        entryEff (sch, schFn) =
          let start = fromIntegral $ (schedPeriod sch - 1)
                      -- FIXME: Assign correct type to counter
              ty = Ty.TyWord Ty.Word16
              -- Counter variable:
              var = AST.ExpSym $ memSym $ counter sch
              -- Predicate, true if counter equals zero:
              counterZero = AST.ExpOp (AST.ExpEq ty)
                            [var, AST.ExpLit $ AST.LitInteger 0]
              -- True case:
              callSched = AST.Call Ty.TyVoid Nothing
                          (AST.NameSym $ procName schFn) []
              resetCount = AST.Store ty var $ AST.ExpLit $ AST.LitInteger start
              -- False case:
              decrCount = AST.Store ty var $
                          (AST.ExpOp AST.ExpSub
                           [var, AST.ExpLit $ AST.LitInteger 1])
              -- FIXME: Both true and false case have a problem, which is that
              -- AST.Store is storing to a reference, and 'var' is not a
              -- pointer.
          in emit $ AST.IfTE counterZero [callSched, resetCount] [decrCount]
        entryProc = proc "start_ion_" $ body $ do
          mapM_ entryEff $ zip scheds schedFns
          -- TODO: Disambiguate the name of this procedure
-- This perhaps should be seen as an analogue of 'writeC' in Code.hs in Atom.

-- | Produce an Ivory effect from a 'Schedule'.
getIvory :: (eff ~ NoEffects) => Schedule -> Ivory eff ()
-- Originally:
-- (GetBreaks eff ~ NoBreak, GetReturn eff ~ NoReturn, GetAlloc eff ~ NoAlloc)
getIvory i0 = do
  comment $ "Name: " ++ schedName i0
  comment $ "Path: " ++ (foldl1 (\s acc -> (s ++ "." ++ acc)) $ schedPath i0)
  comment $ "Phase: " ++ (show $ schedPhase i0)
  comment $ "Period: " ++ (show $ schedPeriod i0)
  sequence_ $ schedAction i0
