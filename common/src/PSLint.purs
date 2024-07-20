module PSLint where

import Prelude

import Data.Array.NonEmpty as NArray
import Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
-- import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer.Class as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Glob.Basic (expandGlobsCwd)
import PSLint.Types ( PSLintConfig)
import Utils (PSLint)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types as CST

lint :: forall b. PSLintConfig -> (String -> b) -> Aff (Array b)
lint psLintConfig fn = do
  files <- expandGlobsCwd psLintConfig.files
  ignoreFiles <- expandGlobsCwd psLintConfig.ignore
  let filteredFiles = S.difference files ignoreFiles
  -- let _ = spy "PSLint files : " $ S.toUnfoldable files :: Array String
  -- let _ = spy "PSLint Ignore files : " $ S.toUnfoldable ignoreFiles :: Array String
  -- let _ = spy "PSLint Filtered files : " $ S.toUnfoldable filteredFiles :: Array String
  traverse (\file_path -> do
    content <- (liftEffect <<< Buffer.toString UTF8) =<< readFile file_path
    pure $ fn content
  ) $ S.toUnfoldable filteredFiles 




lintModule :: PSLint -> String -> Array (String /\ CST.SourceRange)
lintModule config content = 
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