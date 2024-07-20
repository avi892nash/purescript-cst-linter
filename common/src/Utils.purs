module Utils where

import Prelude

import Data.Tuple.Nested (type (/\))
import PureScript.CST.Types as CST


type PSLint = {
  lintModule :: CST.Module Void -> (Array (String /\ CST.SourceRange)),
  lintDecl :: CST.Declaration Void -> (Array (String /\ CST.SourceRange)),
  lintBinder :: CST.Binder Void -> (Array (String /\ CST.SourceRange)),
  lintExpr :: CST.Expr Void -> (Array (String /\ CST.SourceRange)),
  lintType :: CST.Type Void -> (Array (String /\ CST.SourceRange))
}

defaultLinter :: PSLint
defaultLinter = 
  { lintModule : const [],
    lintDecl : const [],
    lintBinder : const [],
    lintExpr : const [],
    lintType : const []
  }