{- |
Module: IonUtil
Description: Utility functions for Ion
Copyright: (c) 2015 Chris Hodapp

-}
module IonUtil where

import           Ivory.Language
import           Ivory.Language.Proc ( Def(..), IvoryCall_ )
import qualified Ivory.Language.Syntax.AST as AST

procName :: Def proc -> String
procName def = case def of
  DefProc p   -> AST.procSym p
  DefImport i -> AST.importSym i
