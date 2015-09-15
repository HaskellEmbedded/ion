{- |
Module: Code
Description: Ivory code generation from Ion specifications
Copyright: (c) 2015 Chris Hodapp

This contains functionality for converting the 'Ion' type to Ivory constructs.

Known issues:

   * One must depend on the Ivory module that makes use of the
definitions from 'ionDef' in order to reference a variable declared
with 'area''.
   * It can be really inefficient to require a separate counter for
every distinct phase within a period.  Why not reuse variables here
when it's within the same period, and rather than starting at the
phase, counting down, and checking for zero, instead starting just one
variable at 0, counting up, checking for each individual phase?

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Ivory.Language.Ion.Code where

import           Control.Monad.State hiding ( forever )
import           Control.Monad.Writer

import           Ivory.Language
import           Ivory.Language.MemArea ( memSym )
import           Ivory.Language.Monad ( emit )
import qualified Ivory.Language.Syntax.AST as AST
import qualified Ivory.Language.Syntax.Names as N
import qualified Ivory.Language.Syntax.Type as Ty

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Schedule
import           Ivory.Language.Ion.Util

-- | Concrete exports from an 'Ion'
data IonExports a = IonExports
                    { ionEntry :: Def ('[] ':-> ())
                    , ionModule :: ModuleDef
                    , ionValue :: a
                    }
-- FIXME: Figure out why I must have 'ModuleDef' and a value twice.
-- I'm basically just exporting an 'Ion' (but one that semantically is
-- different) plus an entry procedure.

-- | Produce exports from the given 'Ion' specs.
ionDef :: String -- ^ Name for schedule function
          -> Ion a -- ^ Ion specification
          -> IonExports a
ionDef name s = IonExports { ionEntry = entryProc
                           , ionModule = mod
                           , ionValue = fst v
                           }
  where init = SeqState { seqId = name, seqNum = 0 }
        -- FIXME: 'init' should probably not be hard-coded.
        -- i0 :: Ion (a, SeqState)
        i0 = runStateT s init
        (v, def) = runWriter i0
        mod = do ionDefs def
                 incl entryProc
                 mapM_ incl schedFns
                 mapM_ counterDef nodes
        nodes = flatten i0
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
        id' sch = "_" ++ (show $ schedId sch)
        -- The name of the counter symbol:
        counterSym sch = "counter_" ++ schedName sch ++ id' sch
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
            -- FIXME: I think this introduces problems when phase proceeds
            -- period, and phase exceeds a Word8.
        -- The Ivory procedure for some schedule item:
        mkSchedFn sch = proc ("ion_" ++ schedName sch ++ id' sch) $ body $ do
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
    [] -> do comment "Action has no conditions"
             actions
    -- Otherwise, evaluate & logical AND them all:
    condEffs -> do
      comment $ "Action has " ++ (show $ length condEffs) ++ " conditions:"
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
