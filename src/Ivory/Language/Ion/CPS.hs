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

-- Another function that will be much more difficult to implement:
join :: (a -> b -> c) -> IonCont t '[a] -> IonCont t '[b] -> IonCont t '[c]
join _ _ _ = undefined

-- This would implement a 'join point' of sorts.  The returned IonCont
-- would not call its own continuation until the other two continuations
-- (those of the first two IonCont arguments) have been called.  The entry
-- function should call that of both of the arguments.
