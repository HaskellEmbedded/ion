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

-- | Generate an Ivory schedule procedure and needed 'ModuleDef' from the given
-- Ion spec.
ionDef :: Ion () -> (Def ('[] ':-> ()), ModuleDef)
ionDef i0 = (entryProc, mod)
  where mod = do incl entryProc
                 mapM_ incl schedFns
                 mapM_ counterDef nodes
        nodes = flatten defaultSchedule $ head $ ionNodes i0
        -- FIXME: This shouldn't just be taking the head node, and we should
        -- probably also not hard-code defaultSchedule.
        -- The entry procedure for running the schedule:
        entryProc :: Def ('[] ':-> ())
        entryProc = proc "start_ion_" $ body $ do
          mapM_ entryEff $ zip nodes schedFns
          -- FIXME: Disambiguate the name of this procedure
        schedFns :: [Def ('[] ':-> ())]
        schedFns = map mkSchedFn nodes
        -- The name of the counter symbol:
        counterSym sch = "counter_" ++ schedName sch
        -- The ModuleDef of the counter's MemArea:
        counterDef sch =
          let areaDef :: forall a .
                         (IvoryType a, IvoryInit a, IvoryZeroVal a, Num a) =>
                         Proxy a -> ModuleDef
              areaDef _ = defMemArea $ area (counterSym sch) $ Just $ ival $
                          ((fromIntegral $ schedPhase sch) :: a)
          in case (fitWordType $ schedPeriod sch) of
            (Ty.TyWord Ty.Word8)  -> areaDef (Proxy :: Proxy Uint8)
            (Ty.TyWord Ty.Word16) -> areaDef (Proxy :: Proxy Uint16)
            (Ty.TyWord Ty.Word32) -> areaDef (Proxy :: Proxy Uint32)
            (Ty.TyWord Ty.Word64) -> areaDef (Proxy :: Proxy Uint64)
            -- FIXME: Is there a cleaner way to do the above?
        -- The Ivory procedure for some schedule item:
        mkSchedFn sch = proc ("ion_" ++ schedName sch) $ body $ do
          noReturn $ noBreak $ noAlloc $ getIvory sch
        -- The Ivory effect for invoking a given schedule item:
        entryEff (sch, schFn) = emit $
                                AST.IfTE counterZero [callSched, reset] [decr]
          where ty = fitWordType $ schedPeriod sch
                -- Counter variable:
                var = AST.ExpSym $ counterSym sch
                -- Pointer to it (because AST.Store assumes a reference):
                var' = AST.ExpAddrOfGlobal $ counterSym sch
                -- Predicate, true if counter equals zero:
                counterZero = AST.ExpOp (AST.ExpEq ty)
                              [var, AST.ExpLit $ AST.LitInteger 0]
                -- True case (counter = 0):
                callSched = AST.Call Ty.TyVoid Nothing
                            (AST.NameSym $ procName schFn) []
                reset = AST.Store ty var' $ AST.ExpLit $
                        AST.LitInteger $ fromIntegral (schedPeriod sch - 1)
                -- False case:
                decr = AST.Store ty var' $
                       (AST.ExpOp AST.ExpSub
                        [var, AST.ExpLit $ AST.LitInteger 1])
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
