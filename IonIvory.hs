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
ionDef :: String -- ^ Name for schedule function
          -> Ion a -- ^ Ion specification
          -> (Def ('[] ':-> ()), ModuleDef) -- ^ (schedule entry procedure,
          -- module definitions)
ionDef name i0 = (entryProc, mod)
  where mod = do ionDefs i0
                 incl entryProc
                 mapM_ incl schedFns
                 mapM_ counterDef nodes
        nodes = flatten $ head $ ionNodes i0
        -- FIXME: This shouldn't just be taking the head node, and we should
        -- probably also not hard-code defaultSchedule.
        -- The entry procedure for running the schedule:
        entryProc :: Def ('[] ':-> ())
        entryProc = proc name $ body $ do
          let nodeComment (sch, _) =
                comment $ "Path: " ++ (foldl1 (\s acc -> (s ++ "." ++ acc)) $
                                       schedPath sch)
          comment "Auto-generated schedule entry procedure from Ion & Ivory"
          mapM_ (\t -> nodeComment t >> entryEff t) $ zip nodes schedFns
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
  comment "Auto-generated schedule procedure from Ion & Ivory"
  comment $ "Path: " ++ (foldl1 (\s acc -> (s ++ "." ++ acc)) $ schedPath i0)
  comment $ "Phase: " ++ (show $ schedPhase i0)
  comment $ "Period: " ++ (show $ schedPeriod i0)
  let actions = sequence_ $ schedAction i0
  case schedCond i0 of
    -- If no conditions, apply actions directly:
    [] -> actions
    -- Otherwise, evaluate & logical AND them all:
    condEffs -> do
      conds <- sequence condEffs
      ifte_ (foldr1 (.&&) conds)
        actions
        $ return ()
      -- FIXME: Short-circuit evaluation might be helpful here.  We don't need
      -- to evaluate any other condition as soon as one has failed.
      -- This might be inefficient for other reasons too - we re-evaluate the
      -- same condition in every single sub-node.
      -- FIXME: Can we evaluate Ivory constants at code generation time and
      -- just fully enable/disable the node then?
