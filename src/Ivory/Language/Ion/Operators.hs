{- |
Module: Operators
Description: Operators used in creating Ion specifications
Copyright: (c) 2015 Chris Hodapp

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.Ion.Operators where

import           Control.Applicative ( (<$>) )
import           Control.Monad
import           Control.Monad.State hiding ( forever )
import qualified Data.Tree as Tree
import           Debug.Trace

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM
import           Ivory.Language.Proc ( Def(..), Proc(..), IvoryCall_,
                                       IvoryProcDef )

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Schedule

addAction :: (Schedule -> Schedule) -> Ion a -> Ion a
addAction fn sub = do
  start <- get
  -- Keep the name & ID of our starting state, but start from no
  -- ModuleDef and no tree:
  let temp = IonDef
             { ionId = ionId start
             , ionNum = ionNum start
             , ionCtxt = fn $ ionCtxt start
                         -- FIXME: This propagates the context, but
                         -- right now it just mimics what
                         -- flatten/flattenTree already do.  I'm not
                         -- totally sure this is correct.  It should
                         -- however be possible to replace 'ionTree'
                         -- and IonAction, and simply generate the
                         -- tree right here.
             , ionDefs = return ()
             , ionTree = []
             , ionSched = [fn $ ionCtxt start]
             }
      (a, def) = runState sub temp
  -- Append the state from the sub-node:
  put $ start { ionNum = ionNum def
              , ionDefs = ionDefs start >> ionDefs def
              , ionTree = [] -- ionTree start ++ [Tree.Node act $ ionTree def]
              , ionSched = ionSched start ++ ionSched def
              }
  return a

getSched :: Ion Schedule
getSched = ionCtxt <$> get

getPhase :: Ion Integer
getPhase = schedPhase <$> ionCtxt <$> get

-- | Specify a name of a sub-node, returning the parent.
ion :: String -- ^ Name
       -> Ion a -- ^ Sub-node
       -> Ion a
ion = addAction . modSchedule . SetName

-- | Specify a relative phase (i.e. a delay past the last phase), returning
-- the parent.  (The sub-node may readily override this phase.)
delay :: Integral i =>
         i -- ^ Relative phase
         -> Ion a -- ^ Sub-node
         -> Ion a
delay = addAction . modSchedule . SetPhase Relative Exact . fromIntegral

-- | Specify a period for a sub-node, returning the parent. (The sub-node may
-- readily override this period.)
period :: Integral i =>
          i -- ^ Period
          -> Ion a -- ^ Sub-node
          -> Ion a
period = addAction . modSchedule . SetPeriod . fromIntegral

-- | Specify a phase for a sub-node, returning the parent. (The sub-node may
-- readily override this phase.)
phase :: Integral i =>
         i -- ^ Phase
         -> Ion a -- ^ Sub-node
         -> Ion a
phase = addAction . modSchedule . SetPhase Absolute Exact . toInteger
-- FIXME: This needs to comprehend the different phase types.

-- | Combinator which simply ignores the node.  This is intended to mask off
-- some part of a spec.
disable :: Ion a -> Ion a
disable = addAction $ modSchedule Disable

-- | Combinator to attach a condition to a sub-node
cond :: IvoryAction IL.IBool -> Ion a -> Ion a
cond = addAction . modSchedule . AddCondition

-- | Turn an Ivory effect into an 'Ion'.
ivoryEff :: IvoryAction () -> Ion ()
ivoryEff iv = addAction (modSchedule (IvoryEff iv)) $ return ()

-- | Retrieve a name that will be unique for this instance.
newName :: Ion String
newName = do state <- get
             let num' = ionNum state
             put $ state { ionNum = num' + 1 }
             return $ ionId state ++ "_" ++ show num'

-- | Allocate a 'IL.MemArea' for this 'Ion', returning a reference to it.
-- If the initial value fails to specify the type of this, then an
-- external signature may be needed (or instead 'areaP'').  If access
-- to this variable is needed outside of the 'Ion' monad, retrieve the
-- reference from an 'Ion' with the 'ionRef' function.
-- The 'ModuleDef' for this will be generated automatically.
area' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> Ion (IL.Ref IL.Global area)
area' name init = do
  let mem = IL.area name init
  state <- get
  put $ state { ionDefs = ionDefs state >> IL.defMemArea mem }
  return $ IL.addrOf mem

-- | Same as 'area'', but with an initial 'IL.Proxy' to disambiguate
-- the area type.
areaP' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         IL.Proxy area -- ^ Proxy (to disambiguate type)
         -> String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> Ion (IL.Ref IL.Global area)
areaP' _ = area'

-- | This is 'area'', but using 'Ion' to create a unique name.
-- (The purpose for this is to help with composing an 'Ion' or
-- instantiating one multiple times.)
newArea :: (IL.IvoryArea area, IL.IvoryZero area) =>
           Maybe (IL.Init area) -> Ion (IL.Ref IL.Global area)
newArea init = mkArea =<< newName
  where mkArea name = area' name init

-- | This is 'areaP'', but using 'Ion' to create a unique name.
newAreaP :: (IL.IvoryArea area, IL.IvoryZero area) =>
            IL.Proxy area -> Maybe (IL.Init area) ->
            Ion (IL.Ref IL.Global area)
newAreaP _ = newArea

-- | This is like Ivory 'proc', but using 'Ion' to give the
-- procedure a unique name.
newProc :: (IvoryProcDef proc impl) => impl -> Ion (Def proc)
newProc impl = do
  name <- newName
  state <- get
  let fn sym = IL.proc sym impl
  put $ state { ionDefs = ionDefs state >> (IL.incl $ fn name) }
  return $ fn name

-- | 'newProc' with an initial 'Proxy' to disambiguate the procedure type
newProcP :: (IvoryProcDef proc impl) =>
            IL.Proxy (Def proc) -> impl -> Ion (Def proc)
newProcP _ = newProc

-- | All the functions below are for generating procedures to adapt a procedure
-- of different numbers of arguments.  I am almost certain that a better way
-- exists than what I did below - probably using typeclasses and mimicking
-- what Ivory did to define the functions.
adapt_0_1 :: (IL.IvoryType a, IL.IvoryVar a) =>
             Def ('[] ':-> ()) -> Ion (Def ('[a] ':-> ()))
adapt_0_1 fn0 = newProc $ \_ -> IL.body $ IL.call_ fn0

adapt_1_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a) =>
             Def ('[a] ':-> ()) -> Ion (Def ('[] ':-> ()))
adapt_1_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0

adapt_0_2 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b) =>
             Def ('[] ':-> ()) -> Ion (Def ('[a,b] ':-> ()))
adapt_0_2 fn0 = newProc $ \_ _ -> IL.body $ IL.call_ fn0

adapt_2_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a, Num b, IL.IvoryType b,
              IL.IvoryVar b) =>
             Def ('[a, b] ':-> ()) -> Ion (Def ('[] ':-> ()))
adapt_2_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0 0

adapt_0_3 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b,
              IL.IvoryType c, IL.IvoryVar c) =>
             Def ('[] ':-> ()) -> Ion (Def ('[a,b,c] ':-> ()))
adapt_0_3 fn0 = newProc $ \_ _ _ -> IL.body $ IL.call_ fn0

adapt_3_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a, Num b, IL.IvoryType b,
              IL.IvoryVar b, Num c, IL.IvoryType c, IL.IvoryVar c) =>
             Def ('[a, b, c] ':-> ()) -> Ion (Def ('[] ':-> ()))
adapt_3_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0 0 0

adapt_0_4 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b,
              IL.IvoryType c, IL.IvoryVar c, IL.IvoryType d, IL.IvoryVar d) =>
             Def ('[] ':-> ()) -> Ion (Def ('[a,b,c,d] ':-> ()))
adapt_0_4 fn0 = newProc $ \_ _ _ _ -> IL.body $ IL.call_ fn0

adapt_4_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a, Num b, IL.IvoryType b,
              IL.IvoryVar b, Num c, IL.IvoryType c, IL.IvoryVar c, Num d,
              IL.IvoryType d, IL.IvoryVar d) =>
             Def ('[a, b, c, d] ':-> ()) -> Ion (Def ('[] ':-> ()))
adapt_4_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0 0 0 0

adapt_0_5 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b,
              IL.IvoryType c, IL.IvoryVar c, IL.IvoryType d, IL.IvoryVar d,
              IL.IvoryType e, IL.IvoryVar e) =>
             Def ('[] ':-> ()) -> Ion (Def ('[a,b,c,d,e] ':-> ()))
adapt_0_5 fn0 = newProc $ \_ _ _ _ _ -> IL.body $ IL.call_ fn0

-- | Create a timer resource.  The returned 'Ion' still must be called at
-- regular intervals (e.g. by including it in a larger Ion spec that is
-- already active).  See 'startTimer' and 'stopTimer' to actually activate this
-- timer.
timer :: (a ~ 'IL.Stored t, Num t, IL.IvoryStore t, IL.IvoryInit t,
          IL.IvoryEq t, IL.IvoryOrd t, IL.IvoryArea a, IL.IvoryZero a) =>
         IL.Proxy t -- ^ Proxy to resolve timer type
         -> Def ('[] ':-> ()) -- ^ Timer expiration procedure
         -> Ion (IL.Ref IL.Global (IL.Stored t))
timer _ expFn = do
  name <- newName

  ion name $ do
    var <- area' name $ Just $ IL.ival 0
    
    ion "decr" $ ivoryEff $ do
      val <- IL.deref var
      IL.ifte_ (val IL.==? 0) (return ()) -- Do nothing if already 0
      -- Otherwise, decrement
        $ do let val' = val - 1
             IL.store var (val')
             -- If it transitions to 0, then call the expiration proc
             IL.ifte_ (val' IL.>? 0) (return ()) $ IL.call_ expFn

    return var
-- FIXME: If the timer expiration procedure is to be fixed at
-- compile-time, maybe I should also just allow Ivory effects.  This
-- might make for lighter code and drop the need to make a new
-- function as a handler.

-- | Begin counting a timer down by the given number of ticks.
startTimer :: (Num t, IL.IvoryStore t, IL.IvoryZeroVal t) =>
              IL.Ref IL.Global (IL.Stored t) -- ^ Timer from 'timer'
              -> Integer -- ^ Countdown time
              -> ILM.Ivory eff ()
startTimer ref n = IL.store ref $ fromInteger n
-- FIXME: Will this even work right in usage?  Think of whether or not the
-- variable will be in scope.  Must these be in the same module?

-- | Stop a timer from running.
stopTimer ref = startTimer ref 0
