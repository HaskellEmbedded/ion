{- |
Module: IonUtil
Description: Utility functions for Ion
Copyright: (c) 2015 Chris Hodapp

-}
module IonUtil where

import           Data.Char ( isAlpha, isDigit )

import           Ivory.Language
import           Ivory.Language.Proc ( Def(..), IvoryCall_ )
import qualified Ivory.Language.Syntax.AST as AST
import qualified Ivory.Language.Syntax.Type as Ty

-- | Return the symbol name of an Ivory procedure
procName :: Def proc -> String
procName def = case def of
  DefProc p   -> AST.procSym p
  DefImport i -> AST.importSym i

-- | Return the Ivory unsigned int type (in its AST) that the given 'Integer'
-- would require (i.e. any value from 0 to 255 returns a 'Ty.Word8'; values
-- beyond that but less than 65535 require a 'Ty.Word16'; and so on.)
-- The given integer must be non-negative.
fitWordType :: Integer -> Ty.Type
fitWordType i = 
  if (i < 0)
  then error ("fitWordType: Integer " ++ show i ++ " is negative.")
  else
    if (i < 2^8) then Ty.TyWord Ty.Word8
    else
      if (i < 2^16) then Ty.TyWord Ty.Word16
      else
        if (i < 2^32) then Ty.TyWord Ty.Word32
        else
          if (i < 2^64) then Ty.TyWord Ty.Word64
          else error ("fitWordType: Integer " ++ show i ++ " is too large.")

-- | Checks the given string for being a valid C identifier.  If it is, then
-- it returns 'Nothing', and otherwise 'Just' and the string index of the
-- character which renders it invalid.
checkCName :: String -> Maybe Int
checkCName [] = Just 0 -- empty identifier is not allowed
checkCName str = check str 0
  where check :: String -> Int -> Maybe Int
        check [] _ = Nothing
        check (c:cs) i = if (isAlpha c || '_' == c || (i > 0 && isDigit c))
                         then check cs (i + 1)
                         else Just i
