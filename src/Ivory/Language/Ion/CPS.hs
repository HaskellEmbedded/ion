{- |
Module: CPS
Description: Ion types for continuations & continuation-passing style
Copyright: (c) 2015 Chris Hodapp

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.Ion.CPS where

import           Ivory.Language

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Operators

-- | This wraps a pattern of functions calling each other in
-- continuation-passing style.  The intent is that the returned entry
-- function (which takes arguments 'a') causes the supplied
-- continuation function to be called (passing arguments 'b').
--
-- This is a common pattern for asynchronous calls, for instance, in
-- which the callback or interrupt calls the continuation function.
--
-- Multiple calls of this sort can be composed with '(=<<)' (and with
-- @RecursiveDo@ and 'mdo') to chain them in the order in which they
-- would proceed.
-- 
-- For instance, in @start <- call1 =<< call2 =<< call3 final@,
-- @start@ contains the entry function to @call1@, whose continuation
-- is set to the entry function of @call2@, whose continuation in turn
-- is set to the entry function of @call3@, whose continuation is
-- 'final'.  Note that chaining these with '(>>=)' is possible too,
-- but the order is somewhat reversed from what is logical - hence,
-- 'mdo' often being sensible here.
type IonCont a b = Def (b ':-> ()) -- ^ Continuation function
                   -> Ion (Def (a ':-> ())) -- ^ Entry function

-- | 'Lift' a Haskell function up into an 'IonCont'.
lift :: (IvoryType a, IvoryVar a, IvoryType b, IvoryVar b) =>
        (a -> b) -> IonCont '[a] '[b]
lift f cont = newProc $ \a -> body $ call_ cont $ f a

-- | 'Accumulate' an argument into a continuation function.
-- Specifically: Given an 'IonCont' taking some argument in its entry
-- function, generate another 'IonCont' with the same type of entry
-- function, but whose continuation function contains another argument
-- (which will receive the same value of that argument).
-- 
-- Note that every use of this requires a static variable of type 'a'.
-- Also, this implementation does not protect against the continuation
-- function being called without the entry function; if this occurs,
-- the continuation will contain old values of 'a' from earlier
-- invocations, or possibly a zero value.
--
-- TODO: Right now this handles only converting single-argument to
-- double-argument.  I intend to modify this to work similarly to
-- 'call' and 'callAux' in Ivory.
accum :: (IvoryType a, IvoryVar a, IvoryStore a, IvoryZeroVal a,
          IvoryType b, IvoryVar b) =>
         IonCont '[] '[b] -> IonCont '[a] (a ': '[b]) 
accum f_ab cont = do
  -- Temporary variable to hold 'a' while waiting to be called back:
  tempA <- newArea Nothing

  -- Generate a new continuation which calls the continuation with the
  -- temporary 'a' value:
  cont2 <- newProc $ \b -> body $ do
    a <- deref tempA
    call_ cont a b

  -- 'entry2' is the entry function using 'cont2' as the continuation:
  entry2 <- f_ab cont2

  -- And finally, the new entry function:
  entry <- newProc $ \a -> body $ do
    store tempA a
    call_ entry2
    
  return entry

-- Another function that will be much more difficult to implement:
join :: (a -> b -> c) -> IonCont t '[a] -> IonCont t '[b] -> IonCont t '[c]
join _ _ _ = undefined

-- This would implement a 'join point' of sorts.  The returned IonCont
-- would not call its own continuation until the other two continuations
-- (those of the first two IonCont arguments) have been called.  The entry
-- function should call that of both of the arguments.
