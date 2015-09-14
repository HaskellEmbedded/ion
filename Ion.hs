{- |
Module: Ion
Description: Top-level Ion module
Copyright: (c) 2015 Chris Hodapp

Ion is a Haskell EDSL that is inspired by another EDSL,
<https://hackage.haskell.org/package/atom Atom>.  Ion aims to be a
re-implementation of Atom which, rather than generating C code
directly (as Atom does), interfaces with another very powerful, more
general EDSL, <http://ivorylang.org/ Ivory>.

It also contains support for sequencing and bundling related
procedures together, particularly for cases when they call each other
in continuation-passing style or rely on asynchronous callbacks - and
need to be composed.

To-do items:

   * Solve the annoying bugs mentioned in IonTest!  A good starting point
appears to be the 'SpecWith' type in hspec,
https://hackage.haskell.org/package/hspec
   * Put this code into sensible namespaces!
   * Continue writing documentation and examples!
   * I need to convert over the 'schedule' function in Scheduling.hs in Atom.
(This is partially done in 'flatten'.)
   * I can do a relative phase; what about a relative period? That is, a
period which is relative not to the base rate, but to the last rate that was
inherited.
   * Atom treats everything within a node as happening at the same time, and I
do not handle this yet, though I rather should.  This may be complicated - I
may either need to process the Ivory effect to look at variable references, or
perhaps add certain features to the monad.
   * Right now one can only pass variables to an Ion by way of a Ref or some
derivative, and those must then be dereferenced inside of an 'ivoryEff' call.
Is this okay?  Should we make this more flexible somehow?  (I feel like Atom
did it similarly, with V & E.)
   * Pretty-printing the schedule itself (as Atom does) would probably be a
good idea.
   * Replacing the existing Ion monad with some kind of free monad might
simplify and clarify the code.
   * Atom contained a way to retrieve the current period and phase inside the
monad; I should implement this.
   * There is *still* a problem with phase and period 'leaking' between
consecutive actions in the monad!
   * Consider the case where you put a condition on a node, and that node
has many sub-nodes across various delays.  Now, suppose that that condition
becomes false somewhere in the middle of those delays.  Is the entire node
blocked from taking effect, or does it partially take effect?  When is the
condition considered as being evaluated?  Right now it is evaluated at every
single sub-node that inherits it.  I consider this to be a violation of how
Ion should operate - synchronously and atomically.

Things to consider (copied from ProcSeq):

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

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ion where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State hiding ( forever )
import           Control.Monad.Writer
import           Data.Maybe ( mapMaybe )
import           Data.Typeable

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM
import           Ivory.Language.Proc ( Def(..), Proc(..), IvoryCall_,
                                       IvoryProcDef )


import           IonMonad
import           IonUtil

-- | This wraps 'Ion' with the ability to create unique C identifier names.
type IonSeq t = StateT SeqState Ion t

data SeqState = SeqState { seqId :: String -- ^ Unique ID (used as base name)
                         , seqNum :: Int -- ^ Next unused number
                         } deriving (Show)

addAction :: IonAction -> IonSeq a -> IonSeq a
addAction act s0 = do
  -- Get the current state, and transform it with the input 'IonSeq':
  m <- runStateT s0 <$> get
  -- Set the next state from the ending state of 'runStateT':
  let (v, _) = runWriter m
  put $ snd v
  -- and insert the generated 'Ion':
  lift $ addAction_ act $ liftM fst m
  -- FIXME: Clean up the above and make it easier to comprehend

-- | Specify a name of a sub-node, returning the parent.
ion :: String -- ^ Name
       -> IonSeq a -- ^ Sub-node
       -> IonSeq a
ion = addAction . SetName

-- | Specify a relative phase (i.e. a delay past the last phase), returning
-- the parent.  (The sub-node may readily override this phase.)
delay :: Integral i =>
         i -- ^ Relative phase
         -> IonSeq a -- ^ Sub-node
         -> IonSeq a
delay = addAction . SetPhase Relative Exact . fromIntegral

-- | Specify a period for a sub-node, returning the parent. (The sub-node may
-- readily override this period.)
period :: Integral i =>
          i -- ^ Period
          -> IonSeq a -- ^ Sub-node
          -> IonSeq a
period = addAction . SetPeriod . fromIntegral

-- | Specify a phase for a sub-node, returning the parent. (The sub-node may
-- readily override this phase.)
phase :: Integral i =>
         i -- ^ Phase
         -> IonSeq a -- ^ Sub-node
         -> IonSeq a
phase = addAction . SetPhase Absolute Min . toInteger
-- FIXME: This needs to comprehend the different phase types.

-- | Combinator which simply ignores the node.  This is intended to mask off
-- some part of a spec.
disable :: IonSeq a -> IonSeq a
disable = addAction Disable

-- | Combinator to attach a condition to a sub-node
cond :: IvoryAction IL.IBool -> IonSeq a -> IonSeq a
cond = addAction . AddCondition

-- | Turn an Ivory effect into an 'Ion'.
ivoryEff :: IvoryAction () -> IonSeq ()
ivoryEff iv = addAction (IvoryEff iv) $ return ()

-- | Retrieve a name that will be unique for this instance.
newName :: IonSeq String
newName = do state <- get
             let num' = seqNum state
             put state { seqNum = num' + 1 }
             return $ seqId state ++ "_" ++ show num'

-- | Allocate a 'IL.MemArea' for this 'Ion', returning a reference to it.
-- If the initial value fails to specify the type of this, then an
-- external signature may be needed (or instead 'areaP'').  If access
-- to this variable is needed outside of the 'Ion' monad, retrieve the
-- reference from an 'Ion' with the 'ionRef' function.
-- The 'ModuleDef' for this will be generated automatically.
area' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> IonSeq (IL.Ref IL.Global area)
area' name init = do
  let mem = IL.area name init
  tell (IL.defMemArea mem, [])
  return $ IL.addrOf mem

-- | Same as 'area'', but with an initial 'IL.Proxy' to disambiguate
-- the area type.
areaP' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         IL.Proxy area -- ^ Proxy (to disambiguate type)
         -> String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> IonSeq (IL.Ref IL.Global area)
areaP' _ = area'

-- | This is 'area'', but using 'IonSeq' to create a unique name.
-- (The purpose for this is to help with composing an 'IonSeq' or
-- instantiating one multiple times.)
newArea :: (IL.IvoryArea area, IL.IvoryZero area) =>
           Maybe (IL.Init area) -> IonSeq (IL.Ref IL.Global area)
newArea init = mkArea =<< newName
  where mkArea name = area' name init

-- | This is 'areaP'', but using 'IonSeq' to create a unique name.
newAreaP :: (IL.IvoryArea area, IL.IvoryZero area) =>
            IL.Proxy area -> Maybe (IL.Init area) ->
            IonSeq (IL.Ref IL.Global area)
newAreaP _ = newArea

-- | This is like Ivory 'proc', but using 'IonSeq' to give the
-- procedure a unique name.
newProc :: (IvoryProcDef proc impl) => impl -> IonSeq (Def proc)
newProc impl = do
  name <- newName
  let fn sym = IL.proc sym impl
  tell (IL.incl $ fn name, [])
  return $ fn name

-- | 'newProc' with an initial 'Proxy' to disambiguate the procedure type
newProcP :: (IvoryProcDef proc impl) =>
            IL.Proxy (Def proc) -> impl -> IonSeq (Def proc)
newProcP _ = newProc

-- | All the functions below are for generating procedures to adapt a procedure
-- of different numbers of arguments.  I am almost certain that a better way
-- exists than what I did below - probably using typeclasses and mimicking
-- what Ivory did to define the functions.
adapt_0_1 :: (IL.IvoryType a, IL.IvoryVar a) =>
             Def ('[] ':-> ()) -> IonSeq (Def ('[a] ':-> ()))
adapt_0_1 fn0 = newProc $ \_ -> IL.body $ IL.call_ fn0

adapt_1_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a) =>
             Def ('[a] ':-> ()) -> IonSeq (Def ('[] ':-> ()))
adapt_1_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0

adapt_0_2 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b) =>
             Def ('[] ':-> ()) -> IonSeq (Def ('[a,b] ':-> ()))
adapt_0_2 fn0 = newProc $ \_ _ -> IL.body $ IL.call_ fn0

adapt_2_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a, Num b, IL.IvoryType b,
              IL.IvoryVar b) =>
             Def ('[a, b] ':-> ()) -> IonSeq (Def ('[] ':-> ()))
adapt_2_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0 0

adapt_0_3 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b,
              IL.IvoryType c, IL.IvoryVar c) =>
             Def ('[] ':-> ()) -> IonSeq (Def ('[a,b,c] ':-> ()))
adapt_0_3 fn0 = newProc $ \_ _ _ -> IL.body $ IL.call_ fn0

adapt_3_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a, Num b, IL.IvoryType b,
              IL.IvoryVar b, Num c, IL.IvoryType c, IL.IvoryVar c) =>
             Def ('[a, b, c] ':-> ()) -> IonSeq (Def ('[] ':-> ()))
adapt_3_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0 0 0

adapt_0_4 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b,
              IL.IvoryType c, IL.IvoryVar c, IL.IvoryType d, IL.IvoryVar d) =>
             Def ('[] ':-> ()) -> IonSeq (Def ('[a,b,c,d] ':-> ()))
adapt_0_4 fn0 = newProc $ \_ _ _ _ -> IL.body $ IL.call_ fn0

adapt_4_0 :: (Num a, IL.IvoryType a, IL.IvoryVar a, Num b, IL.IvoryType b,
              IL.IvoryVar b, Num c, IL.IvoryType c, IL.IvoryVar c, Num d,
              IL.IvoryType d, IL.IvoryVar d) =>
             Def ('[a, b, c, d] ':-> ()) -> IonSeq (Def ('[] ':-> ()))
adapt_4_0 fn0 = newProc $ IL.body $ IL.call_ fn0 0 0 0 0

adapt_0_5 :: (IL.IvoryType a, IL.IvoryVar a, IL.IvoryType b, IL.IvoryVar b,
              IL.IvoryType c, IL.IvoryVar c, IL.IvoryType d, IL.IvoryVar d,
              IL.IvoryType e, IL.IvoryVar e) =>
             Def ('[] ':-> ()) -> IonSeq (Def ('[a,b,c,d,e] ':-> ()))
adapt_0_5 fn0 = newProc $ \_ _ _ _ _ -> IL.body $ IL.call_ fn0

-- | Create a timer resource.  The returned 'Ion' still must be called at
-- regular intervals (e.g. by including it in a larger Ion spec that is
-- already active).  See 'startTimer' and 'stopTimer' to actually activate this
-- timer.
timer :: (a ~ 'IL.Stored t, Num t, IL.IvoryStore t, IL.IvoryInit t,
          IL.IvoryEq t, IL.IvoryOrd t, IL.IvoryArea a, IL.IvoryZero a) =>
         IL.Proxy t -- ^ Proxy to resolve timer type
         -> Def ('[] ':-> ()) -- ^ Timer expiration procedure
         -> IonSeq (IL.Ref IL.Global (IL.Stored t))
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
