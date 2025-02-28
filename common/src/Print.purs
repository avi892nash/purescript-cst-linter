module Print where

import Prelude

import Data.Foldable (foldMap)
import PureScript.CST (printModule)
import PureScript.CST.Print as Print
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types 


class PrintCST a where
  print :: a -> String

instance PrintCST Ident where
  print (Ident a) = a
else instance PrintCST (Module Void) where
  print = printModule
else instance TokensOf a => PrintCST a where
  print a = foldMap Print.printSourceToken (TokenList.toArray (tokensOf a))


