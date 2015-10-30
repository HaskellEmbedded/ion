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
