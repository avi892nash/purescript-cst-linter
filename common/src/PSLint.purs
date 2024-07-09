module PSLint where

import Data.Tuple.Nested (type (/\), (/\))
import Prelude

import Data.Array.NonEmpty as NArray
import Data.Either (Either(..))
import Data.Set as S
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Buffer.Class as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Glob.Basic (expandGlobsCwd)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types as CST


type PSLint = {
  src :: String, -- "src/**/*.purs"
  lintModule :: CST.Module Void -> (Array (String /\ CST.SourceRange)),
  lintDecl :: CST.Declaration Void -> (Array (String /\ CST.SourceRange)),
  lintBinder :: CST.Binder Void -> (Array (String /\ CST.SourceRange)),
  lintExpr :: CST.Expr Void -> (Array (String /\ CST.SourceRange)),
  lintType :: CST.Type Void -> (Array (String /\ CST.SourceRange))
}

runPSLint :: PSLint -> (Either String (Array (String /\ CST.SourceRange)) -> Effect Unit) -> Effect Unit
runPSLint config fn = launchAff_ $ do 
    files :: Array String <- (pure <<< S.toUnfoldable) =<< expandGlobsCwd [ config.src ]
    if files == []
      then liftEffect $ fn (Left ("No Files found with src : " <> config.src))
      else do
        _ <-traverse (\file_path -> do 
            content <- (liftEffect <<< Buffer.toString UTF8) =<< readFile file_path
            liftEffect $ fn $ Right $ lintModule content config
            pure unit
            ) files
        pure unit



lintModule :: String -> PSLint -> Array (String /\ CST.SourceRange)
lintModule content config = 
  case parseModule content of
    ParseSucceeded m -> 
      (config.lintModule m) <> 
      (foldMapModule $ 
          { onExpr : config.lintExpr
          , onType : config.lintType
          , onBinder : config.lintBinder
          , onDecl : config.lintDecl
          }) m
    ParseSucceededWithErrors _ err -> map (\{error, position} -> (printParseError error) /\ { start : {line : position.line + 1, column : position.column + 1}, end : {line : position.line + 1, column : position.column + 1}}) $ NArray.toArray err
    ParseFailed {error, position} -> [(printParseError error) /\ { start : {line : position.line + 1, column : position.column + 1}, end : {line : position.line + 1, column : position.column + 1}}]