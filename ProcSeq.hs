{- |
Module: ProcSeq
Description: Sequenced/bundled procedures
Copyright: (c) 2015 Chris Hodapp

This is an experimental module for sequencing and bundling related procedures
together, particularly for cases when they call each other in
continuation-passing style or rely on asynchronous callbacks - and need to be
composed.

I am still not entirely sure how much this belongs in Ion.

Things to consider:

   * How would I represent a long non-blocking delay in this?
   * It's still a bit cumbersome when combining together Ivory procedures of
different types, though my 'adapt_x_y' calls help somewhat.
   * In my SPI example, I send an opcode along with a length and an expected
length to read back.  This call is async, and the return continuation receives
the number of bytes actually read.  I want to check this number at the return
continuation - and I'd like to avoid having to write this manually for every
case and repeat the number of expected bytes.  How would I represent this?
   * I still don't have a solution for making 'Ion' and 'ProcSeq' play nice
together.  Particularly: 'ProcSeq', to use something like a timer, must get one
of its functions embedded in an 'Ion' somehow.  This can't be passed as a C
value, since the value would have to persist past a function's lifetime, and
Ivory doesn't permit storing a function pointer in a global.  Second: The 'Ion'
must be accessible outside of the 'ProcSeq' so that it can either have its code
and its entry function generated directly, or else be embedded in some larger
'Ion' spec which takes care of this.  Generally, this means two things must
come out of that 'ProcSeq': an 'Ion' spec that takes care of the timer, and
an entry function (because how else would one access any of its
functionality?).
   * Perhaps merging them into one monad is the thing to do here.  Would
using 'StateT' to transform the 'Ion' monad, rather that just using 'State'
by itself, help this?

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ProcSeq ( ProcSeq
               , seqDef
               , newName
               , newProc
               , newProcP
               , newArea
               , newAreaP
               , timer
               , startTimer
               , stopTimer
               , adapt_0_1
               , adapt_1_0
               , adapt_0_2
               , adapt_2_0
               , adapt_0_3
               , adapt_3_0
               , adapt_0_4
               , adapt_4_0
               , adapt_0_5
               , ion_
               ) where

import           Control.Applicative ( Const )
import           Control.Monad.State hiding ( forever )
import           Data.Proxy ( Proxy )

import           Ivory.Language hiding ( Proxy )
import           Ivory.Language.MemArea
import           Ivory.Language.Proc ( Def(..), IvoryCall_, IvoryProcDef )

import           Ion

-- | This type assists with bundling together sequences of call and state,
-- while creating unique names so that multiple instances do not conflict.
type ProcSeq t = StateT SeqState Ion t

-- | State that is passed along in a 'ProcSeq' which accumulates 'ModuleDef'
-- and increments numbers to generate unique names
data SeqState = SeqState { seqId :: String -- ^ Unique (per instance) ID
                         , seqNum :: Int -- ^ Next unused number
                         , seqDefs :: ModuleDef
                         }

-- | Return the procedure for a 'ProcSeq' and the acculumated 'ModuleDef',
-- given a unique string for an ID.
seqDef :: ProcSeq (Def proc) -> String -> (Def proc, ModuleDef)
seqDef s id = (fn, seqDefs st)
  where (fn, st) = ionVal $ runStateT s init
        init = SeqState { seqId = id, seqNum = 0, seqDefs = return () }

-- | Retrieve a name that will be unique for this instance.
newName :: ProcSeq String
newName = do
  state <- get
  let num' = seqNum state
  put state { seqNum = num' + 1 }
  return $ seqId state ++ "_" ++ show num'

-- | Like Ivory 'proc', but leaving out the first argument (it derives the
-- name from 'ProcSeq').
newProc :: (IvoryProcDef proc impl) => impl -> ProcSeq (Def proc)
newProc impl = do
  name <- newName
  let fn = proc name impl
  modify (\s -> s { seqDefs = seqDefs s >> incl fn })
  return fn

-- | 'newProc' with an initial 'Proxy' to disambiguate the procedure type
newProcP :: (IvoryProcDef proc impl) =>
            Proxy (Def proc) -> impl -> ProcSeq (Def proc)
newProcP _ = newProc

-- | Like Ivory 'area', but leaving out the first argument (it derives the
-- name from 'ProcSeq').
newArea :: (IvoryArea area, IvoryZero area) =>
           Maybe (Init area) -> ProcSeq (MemArea area)
newArea init = do
  name <- newName
  let a = area name init
  modify (\s -> s { seqDefs = seqDefs s >> defMemArea a })
  return a

-- | 'newArea' with an initial 'Proxy' to disambiguate the area type
newAreaP :: (IvoryArea area, IvoryZero area) =>
            Proxy area -> Maybe (Init area) -> ProcSeq (MemArea area)
newAreaP _ = newArea

-- | Create a timer resource.  The returned 'Ion' still must be called at
-- regular intervals (e.g. by including it in a larger Ion spec that is
-- already active).  See 'startTimer' and 'stopTimer' to actually activate this
-- timer.
timer :: (a ~ 'Stored t, Num t, IvoryStore t, IvoryInit t, IvoryEq t,
          IvoryOrd t, IvoryArea a, IvoryZero a) =>
         Proxy t -- ^ Proxy to resolve timer type
         -> Def ('[] ':-> ()) -- ^ Timer expiration procedure
         -> ProcSeq (Ion (Ref Global (Stored t)))
timer _ expFn = do
  name <- newName
  return $ ion name $ do
    var <- area' name $ Just $ ival 0
    
    ion "decr" $ ivoryEff $ do
      val <- deref var
      ifte_ (val ==? 0) (return ()) -- Do nothing if already 0
      -- Otherwise, decrement
        $ do let val' = val - 1
             store var (val')
             -- If it transitions to 0, then call the expiration proc
             ifte_ (val' >? 0) (return ()) $ call_ expFn

    return var

-- | Begin counting a timer down by the given number of ticks.
startTimer :: (Num t, IvoryStore t, IvoryZeroVal t) =>
              Ion (Ref Global (Stored t)) -- ^ Timer from 'timer'
              -> Integer -- ^ Countdown time
              -> Ivory eff ()
startTimer ref n = store (ionRef ref) $ fromInteger n
-- FIXME: Will this even work right in usage?  Think of whether or not the
-- variable will be in scope.  Must these be in the same module?

-- | Stop a timer from running.
stopTimer ref = startTimer ref 0

-- | All the functions below are for generating procedures to adapt a procedure
-- of different numbers of arguments.  I am almost certain that a better way
-- exists than what I did below - probably using typeclasses and mimicking
-- what Ivory did to define the functions.
adapt_0_1 :: (IvoryType a, IvoryVar a) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a] ':-> ()))
adapt_0_1 fn0 = newProc $ \_ -> body $ call_ fn0

adapt_1_0 :: (Num a, IvoryType a, IvoryVar a) =>
             Def ('[a] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_1_0 fn0 = newProc $ body $ call_ fn0 0

adapt_0_2 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b] ':-> ()))
adapt_0_2 fn0 = newProc $ \_ _ -> body $ call_ fn0

adapt_2_0 :: (Num a, IvoryType a, IvoryVar a, Num b, IvoryType b, IvoryVar b) =>
             Def ('[a, b] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_2_0 fn0 = newProc $ body $ call_ fn0 0 0

adapt_0_3 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b, IvoryType c,
              IvoryVar c) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b,c] ':-> ()))
adapt_0_3 fn0 = newProc $ \_ _ _ -> body $ call_ fn0

adapt_3_0 :: (Num a, IvoryType a, IvoryVar a, Num b, IvoryType b, IvoryVar b,
              Num c, IvoryType c, IvoryVar c) =>
             Def ('[a, b, c] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_3_0 fn0 = newProc $ body $ call_ fn0 0 0 0

adapt_0_4 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b, IvoryType c,
              IvoryVar c, IvoryType d, IvoryVar d) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b,c,d] ':-> ()))
adapt_0_4 fn0 = newProc $ \_ _ _ _ -> body $ call_ fn0

adapt_4_0 :: (Num a, IvoryType a, IvoryVar a, Num b, IvoryType b, IvoryVar b,
              Num c, IvoryType c, IvoryVar c, Num d, IvoryType d, IvoryVar d) =>
             Def ('[a, b, c, d] ':-> ()) -> ProcSeq (Def ('[] ':-> ()))
adapt_4_0 fn0 = newProc $ body $ call_ fn0 0 0 0 0

adapt_0_5 :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b, IvoryType c,
              IvoryVar c, IvoryType d, IvoryVar d, IvoryType e, IvoryVar e) =>
             Def ('[] ':-> ()) -> ProcSeq (Def ('[a,b,c,d,e] ':-> ()))
adapt_0_5 fn0 = newProc $ \_ _ _ _ _ -> body $ call_ fn0
