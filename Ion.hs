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
   * Perhaps merging them into one monad is the thing to do here.  Would
using 'StateT' to transform the 'Ion' monad, rather that just using 'State'
by itself, help this?

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Ion where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State hiding ( forever )
import           Data.Maybe ( mapMaybe )
import           Data.Typeable

import qualified Ivory.Language as IL
import qualified Ivory.Language.Monad as ILM
import           Ivory.Language.Proc ( Def(..), Proc(..), IvoryCall_,
                                       IvoryProcDef )


import           IonUtil

-- | The monad for expressing an Ion specification.
data Ion a = Ion { ionNodes :: [IonNode] -- ^ An accumulation of nodes; the
                   -- head is considered the 'current' node
                 , ionDefs :: IL.ModuleDef -- ^ 'ModuleDef' for anything that
                   -- needs to be declared for Ivory
                 , ionVal :: a
                 }-- deriving (Show)

-- | This type assists with bundling together sequences of call and state,
-- while creating unique names so that multiple instances do not conflict.
type IonSeq t = StateT SeqState Ion t

-- | State that is passed along in a 'ProcSeq' which accumulates 'ModuleDef'
-- and increments numbers to generate unique names
data SeqState = SeqState { seqId :: String -- ^ Unique (per instance) ID
                         , seqNum :: Int -- ^ Next unused number
                         } deriving (Show)

instance Functor Ion where
  fmap f ion = ion { ionVal = f $ ionVal ion }

instance Applicative Ion where
  pure = return
  (<*>) = ap

instance Monad Ion where
  ion1 >>= fn = ion2 { ionNodes = ionNodes ion1 ++ ionNodes ion2
                     , ionDefs = ionDefs ion1 >> ionDefs ion2
                     }
    where ion2 = fn $ ionVal ion1

  return a = Ion { ionNodes = [], ionVal = a, ionDefs = return () }

instance MonadFix Ion where
  mfix f = let a = f (ionVal a) in a

-- | A Node representing some context in the schedule, and the actions this
-- node includes.  'ionAction' (except for 'IvoryEff' and 'NoAction') applies
-- not just to the current node, but to any child nodes too.  In general,
-- if two actions conflict (e.g. two 'SetPhase' actions with absolute phase),
-- then the innermost one overrides the other.
data IonNode = IonNode { ionAction :: IonAction -- ^ What this node does
                       , ionSub :: [IonNode] -- ^ Child nodes
                       } deriving (Show)

-- | The type of Ivory action that an 'IonNode' can support. Note that this
-- purposely forbids breaking, returning, and allocating.
type IvoryAction = IL.Ivory IL.NoEffects

instance Show (IvoryAction a) where
  show iv = "Ivory NoEffects () [" ++ show block ++ "]"
    where (_, block) =
            ILM.runIvory $ ILM.noReturn $ ILM.noBreak $ ILM.noAlloc iv

-- | An action/effect that a node can have.
data IonAction = IvoryEff (IvoryAction ()) -- ^ The Ivory effects that this
                 -- node should perform
               | SetPhase PhaseContext PhaseType Integer -- ^ Setting phase -
                 -- i.e. the count within a period (thus, an absolute phase
                 -- must range from @0@ up to @N-1@ for period @N@).
               | SetPeriod Integer -- ^ Setting period
               | SetName String -- ^ Setting a name
               | AddCondition (IvoryAction IL.IBool) -- ^ Adding a condition to
                 -- this node which must return 'true' for the node *and* for
                 -- any sub-nodes to execute their actions
               | NoAction -- ^ Do nothing
               deriving (Show)

-- | Phase = Phase PhaseContext PhaseType Int deriving (Show)

data PhaseContext = Absolute -- ^ Phase is relative to the first tick within a
                    -- period
                  | Relative -- ^ Phase is relative to the last phase used
                  deriving (Show)

data PhaseType = Min -- ^ Minimum phase (i.e. at this phase, or any later point)
               | Exact -- ^ Exactly this phase
               deriving (Show)

defaultNode = IonNode { ionAction = NoAction
                      , ionSub = []
                      }

-- | Produce a somewhat more human-readable representation of an 'Ion'
prettyPrint :: IonNode -> IO ()
prettyPrint node = putStrLn $ unlines $ pretty node
  where sub s = join $ map pretty $ ionSub s
        pretty s = [ "IonNode {"
                   , " ionAction = " ++ (show $ ionAction s)
                   ] ++
                   (if null $ ionSub s
                    then []
                    else " ionSub =" : (map ("    " ++) $ sub s)) ++
                   ["}"]

-- | Given a function which transforms an 'IonNode', and a child 'Ion', return
-- the parent 'Ion' containing this node with that transformation applied.
makeSub :: (IonNode -> IonNode) -> Ion a -> Ion a
makeSub fn ion0 = ion0
                  { ionNodes = [(fn defaultNode) { ionSub = ionNodes ion0 }] }

-- | Given an 'IonAction' to apply, and a child 'Ion', return the parent 'Ion'
-- containing this node with that action.
makeSubFromAction :: IonAction -> IonSeq a -> IonSeq a
makeSubFromAction act s0 = do
  g <- get
  lift $ makeSub (\i -> i { ionAction = act }) $ evalStateT s0 g

-- | Specify a name of a sub-node, returning the parent.
ion :: String -- ^ Name
       -> IonSeq a -- ^ Sub-node
       -> IonSeq a
ion = makeSubFromAction . SetName

-- | Same as 'area'', but with an initial 'IL.Proxy' to specify the type in
-- cases where nothing else has specified it, and external type signatures are
-- too annoying.
areaP' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         IL.Proxy area -- ^ Proxy (to disambiguate type)
         -> String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> IonSeq (IL.Ref IL.Global area)
areaP' _ = area'

-- | Allocate a 'IL.MemArea' for this 'Ion', returning a reference to it.
-- If the initial value fails to specify the type of this, then an external
-- signature may be needed (or instead 'areaP'').  If access to this variable
-- is needed outside of the 'Ion' monad, retrieve the reference from an 'Ion'
-- with the 'ionRef' function.
area' :: (IL.IvoryArea area, IL.IvoryZero area) =>
         String -- ^ Name of variable
         -> Maybe (IL.Init area) -- ^ Initial value (or 'Nothing')
         -> IonSeq (IL.Ref IL.Global area)
area' name init = lift $ Ion { ionNodes = []
                             , ionDefs = IL.defMemArea mem
                             , ionVal = IL.addrOf mem
                             }
  where mem = IL.area name init

-- | Return the Ivory 'IL.Ref' from an 'Ion' containing one (for instance, to
-- access the variable from other Ivory code).
--ionRef :: (IL.IvoryArea area, IL.IvoryZero area) =>
--          IonSeq (IL.Ref IL.Global area) -> IL.Ref IL.Global area
--ionRef = ionVal

-- | Specify a phase for a sub-node, returning the parent. (The sub-node may
-- readily override this phase.)
phase :: Integral i =>
         i -- ^ Phase
         -> IonSeq a -- ^ Sub-node
         -> IonSeq a
phase = makeSubFromAction . SetPhase Absolute Min . toInteger
-- FIXME: This needs to comprehend the different phase types.

delay :: Integral i =>
         i -- ^ Relative phase
         -> IonSeq a -- ^ Sub-node
         -> IonSeq a
delay = makeSubFromAction . SetPhase Relative Min . toInteger

-- | Specify a period for a sub-node, returning the parent. (The sub-node may
-- readily override this period.)
period :: Integral i =>
          i -- ^ Period
          -> IonSeq a -- ^ Sub-node
          -> IonSeq a
period = makeSubFromAction . SetPeriod . toInteger

-- | Combinator which simply ignores the node.  This is intended to mask off
-- some part of a spec.
disable :: IonSeq a -> IonSeq a
disable _ = lift $
            Ion { ionNodes = [], ionVal = undefined, ionDefs = return () }

-- | Combinator to attach a condition to a sub-node
cond :: IvoryAction IL.IBool -> IonSeq a -> IonSeq a
cond = makeSubFromAction . AddCondition

-- | Turn an Ivory effect into an 'Ion'.
ivoryEff :: IvoryAction () -> IonSeq ()
ivoryEff iv = lift $
              Ion { ionNodes = [defaultNode { ionAction = IvoryEff iv }]
                  , ionVal = ()
                  , ionDefs = return ()
                  }

-- | A scheduled action.  Phase and period here are absolute, and there are no
-- child nodes.
data Schedule =
  Schedule { schedId :: Integer -- ^ A unique ID for this action
           , schedName :: String -- ^ Name (without any disambiguation applied)
           , schedPath :: [String] -- ^ A list of names giving the trail that
             -- produced this schedule
           , schedPhase :: Integer -- ^ The (absolute & exact) phase of this
             -- action
           , schedPeriod :: Integer -- ^ The period of this action
           , schedAction :: [IvoryAction ()] -- ^ The Ivory effects for this
                            -- action
           , schedCond :: [IvoryAction IL.IBool] -- ^ Ivory effects which all
                          -- must return 'true' for anything in 'schedAction'
                          -- to execute
           }
  deriving (Show)

defaultSchedule = Schedule { schedId = 0
                           , schedName = "root"
                           , schedPath = []
                           , schedPhase = 0
                           , schedPeriod = 1
                           , schedAction = []
                           , schedCond = []
                           }

-- | Transform a 'Schedule' according to an 'IonAction'.
modSchedule :: IonAction -> Schedule -> Schedule
modSchedule (IvoryEff _) s = s
modSchedule (SetPhase t _ ph) s =
  if (ph' >= schedPeriod s)
  then throw $ PhaseExceedsPeriod (schedPath s) ph' (schedPeriod s)
  else s { schedPhase = ph' }
  where ph' = case t of Absolute -> ph
                        Relative -> schedPhase s + ph
modSchedule (SetPeriod p) s = s { schedPeriod = fromIntegral p }
modSchedule (SetName name) s =
  case checkCName name of Just i -> throw $ InvalidCName (schedPath s) name i
                          Nothing -> s { schedName = name
                                       , schedPath = schedPath s ++ [name]
                                       }
modSchedule (AddCondition iv) s = s { schedCond = iv : schedCond s }
modSchedule NoAction s = s
-- FIXME: Handle exact and minimum phase.

-- | Actual 'State' implementation of 'flatten'; the contained state
-- is (starting schedule state, accumulated schedule).
flattenSt :: IonNode -> State (Schedule, [Schedule]) ()
flattenSt node = do
  (ctxt, scheds) <- get
  let -- Update our context with the actions in 'node':
      ctxt' = modSchedule (ionAction node) ctxt
      ctxtNext = ctxt' { schedAction = [] }
      -- Get a unique name:
      name = schedName ctxt' ++ "_" ++ (show $ schedId ctxt')
      -- Get Ivory actions (if any) or else Nothing:
      getIvory n = case ionAction n of IvoryEff iv -> Just iv
                                       _           -> Nothing
  -- Emit schedule items for any children that have Ivory effects (We do this
  -- to combine all effects at once that are under the same parameters.)
  case (mapMaybe getIvory $ ionSub node) of
   -- For the context that we pass forward, clear out old actions:
   [] -> put (ctxt' { schedAction = [] }, scheds)
   -- If we emit a schedule item then also increment ID:
   actions -> put (ctxt' { schedAction = [], schedId = schedId ctxt + 1 }, 
                   newSched : scheds)
    where newSched = ctxt' { schedAction = actions, schedName = name }
  -- And recurse to the sub-nodes!
  mapM_ flattenSt $ ionSub node
  -- However, we must clean up by restoring whatever context we started with:
  modify $ \(c,s) -> (c { schedPath = schedPath ctxt
                        , schedName = schedName ctxt
                        , schedCond = schedCond ctxt
                        }, s)
  -- FIXME: The above step seems possibly wrong.  Am I certain that no other
  -- context besides path and name requires backtracking? 
  -- FIXME: This does not handle exact or minimum phase.

-- | Walk a hierarchical 'IonNode' and turn it into a flat list of
-- scheduled actions.
flatten :: IonNode -> [Schedule]
flatten node = reverse l
  where (_, l) = execState (flattenSt node) (defaultSchedule, [])

data IonException = InvalidCName [String] String Int -- ^ Path, C name, and
                    -- index at which it is invalid
                  | PhaseExceedsPeriod [String] Integer Integer -- ^ Path,
                    -- phase, period
    deriving (Show, Typeable)

instance Exception IonException


-- * Moved from ProcSeq

-- | Return the procedure for a 'IonSeq' and the acculumated 'ModuleDef',
-- given a unique string for an ID.
seqDef :: IonSeq (Def proc) -> String -> (Def proc, IL.ModuleDef)
seqDef s id = (fn, ionDefs s2)
  where s2 = runStateT s init
        (fn, _) = ionVal s2
        init = SeqState { seqId = id, seqNum = 0 }

-- | Retrieve a name that will be unique for this instance.
newName :: IonSeq String
newName = do
  state <- get
  let num' = seqNum state
  put state { seqNum = num' + 1 }
  return $ seqId state ++ "_" ++ show num'

-- | Like Ivory 'proc', but leaving out the first argument (it derives the
-- name from 'IonSeq').
newProc :: (IvoryProcDef proc impl) => impl -> IonSeq (Def proc)
newProc impl = mkProc =<< newName
  where mkProc name = lift $ Ion { ionNodes = []
                                 , ionDefs = IL.incl $ fn name
                                 , ionVal = fn name
                                 }
        fn name = IL.proc name impl

-- | 'newProc' with an initial 'Proxy' to disambiguate the procedure type
newProcP :: (IvoryProcDef proc impl) =>
            Proxy (Def proc) -> impl -> IonSeq (Def proc)
newProcP _ = newProc

-- | Like Ivory 'area', but leaving out the first argument (it derives the
-- name from 'IonSeq').
newArea :: (IL.IvoryArea area, IL.IvoryZero area) =>
           Maybe (IL.Init area) -> IonSeq (IL.Ref IL.Global area)
newArea init = mkArea =<< newName
  where mkArea name = area' name init

-- | 'newArea' with an initial 'Proxy' to disambiguate the area type
newAreaP :: (IL.IvoryArea area, IL.IvoryZero area) =>
            Proxy area -> Maybe (IL.Init area) -> IonSeq (IL.Ref IL.Global area)
newAreaP _ = newArea

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

-- Code purgatory:
{-
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


-}
