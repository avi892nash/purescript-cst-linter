module Rules.DomSyntaxSafety
  ( DomSyntaxConfig(..)
  , Syntax(..)
  , lintDomSyntaxSafety
  )
  where

import Prelude
import Utils (PSLint, defaultLinter)

import Control.Monad.Error.Class (throwError)
import Data.Array (find, head, tail)
import Data.Array.NonEmpty as NE
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (ForeignError(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types as CST
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type DomSyntaxConfig = { exprApp :: Array { name :: String, syntax :: Array Syntax, strict :: Maybe Boolean } }


data Syntax
  = S
  | I
  | A
  | Any

instance WriteForeign Syntax where
  writeImpl S = writeImpl "string"
  writeImpl I = writeImpl "int"
  writeImpl A = writeImpl "array"
  writeImpl Any = writeImpl "any"

instance ReadForeign Syntax where
  readImpl fgn = do
    v <- readImpl fgn :: _ String
    case S.toLower v of
      "string" -> pure S
      "int" -> pure I
      "array" -> pure A
      "any" -> pure Any
      _ -> throwError $ NEL.singleton $ ForeignError "Cannot able to decode syntax. (Use string, int, array or any)"

lintDomSyntaxSafety :: DomSyntaxConfig -> PSLint
lintDomSyntaxSafety doms =
      defaultLinter {
        lintExpr =
          case _ of
            e@(CST.ExprApp (CST.ExprIdent (CST.QualifiedName { name : CST.Ident name })) b) -> Prelude.do
              case find (\{name: n} -> n == S.trim name) doms.exprApp of
                Just { syntax, strict } -> Prelude.do
                  let params = NE.toArray b
                  recursiveParse strict (rangeOf e) params syntax
                _ -> [] 
            _ -> []
      } 
  where
  recursiveParse :: Maybe Boolean -> CST.SourceRange -> Array (CST.AppSpine CST.Expr Void) -> Array Syntax -> Array (String /\ CST.SourceRange)
  recursiveParse strict exprAppRange l s =
    case head l, head s of
      Nothing, Nothing -> []
      Just a, Just sh -> recursiveParse strict exprAppRange (fromMaybe [] $ tail l) (fromMaybe [] $ tail s)  <>
        case sh of
          S -> 
            case a of
              CST.AppTerm (CST.ExprString _ _) -> []
              CST.AppTerm t -> ["Error : expecting this as string from pslint" /\ (rangeOf t)]
              _ -> []
          I -> 
            case a of
              CST.AppTerm (CST.ExprInt _ _) -> []
              CST.AppTerm t -> ["Error : expecting this as int from pslint" /\ (rangeOf t)] 
              _ -> []
          A -> 
            case a of
              CST.AppTerm (CST.ExprArray _) -> []
              CST.AppTerm t -> ["Error : expecting this as array from pslint" /\ (rangeOf t)]
              _ -> []
          Any -> []
      _,_ -> 
        if fromMaybe false strict 
          then ["Number of syntax constraints from pslint are not matching. Considering changing name or removing strict boolean for this" /\ exprAppRange]
          else []
