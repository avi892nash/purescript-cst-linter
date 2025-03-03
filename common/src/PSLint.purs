module PSLint where

import PSLint.Types (PSLintConfig)
import Prelude

import Data.Array.NonEmpty as NArray
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer.Class as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Glob.Basic (expandGlobsCwd)
import PSLint.Types as PSLint
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Traversal (foldMapModule)
import PureScript.CST.Types as CST
import Rules.DomSyntaxSafety (lintDomSyntaxSafety)
import Rules.MandatorySignature (lintMandatorySignature)
import Rules.NoArrayJSX (lintNoArrayJSX)
import Rules.NoClassConstraints (lintNoClassConstraints)
import Utils (PSLint)

lintAllFiles :: PSLintConfig -> Aff (Array { uri :: String, content :: String, params :: Array { status :: String, type :: String, ranges :: Array (String /\ CST.SourceRange) }})
lintAllFiles config = do
  files <- expandGlobsCwd config.files
  ignoreFiles <- expandGlobsCwd config.ignore
  let filteredFiles = S.difference files ignoreFiles
  -- let _ = spy "PSLint files : " $ S.toUnfoldable files :: Array String
  -- let _ = spy "PSLint Ignore files : " $ S.toUnfoldable ignoreFiles :: Array String
  -- let _ = spy "PSLint Filtered files : " $ S.toUnfoldable filteredFiles :: Array String
  filteredFilesContent <- traverse (\file_path -> do
    content <- (liftEffect <<< Buffer.toString UTF8) =<< readFile file_path
    pure $ { uri : "file://" <> file_path, content }
  ) $ S.toUnfoldable filteredFiles
  pure $ 
    map 
      (\{uri, content} -> { uri, content, params : map (\p -> { status : p.status, type : p.type, ranges : p.ranges }) $ lintModule config uri content })
      filteredFilesContent



lintModule :: PSLintConfig -> String -> String -> Array { status :: String, type :: String, uri :: String, ranges :: Array (String /\ CST.SourceRange) }
lintModule config uri content = 
  let mandatorySignatureReport =
        case config.rules."mandatory-signature" of
          Just PSLint.Error -> [{ status : "error", type: "mandatory-signature", uri, ranges : cstMatchModule lintMandatorySignature }]
          Just PSLint.Warn  -> [{ status : "warn", type: "mandatory-signature", uri, ranges : cstMatchModule lintMandatorySignature }]
          _ -> []
      noArrayJSXReport =
        case config.rules."no-array-jsx" of
          Just PSLint.Error -> [{ status : "error", type: "no-array-jsx", uri, ranges : cstMatchModule lintNoArrayJSX }]
          Just PSLint.Warn  -> [{ status : "warn", type: "no-array-jsx", uri, ranges : cstMatchModule lintNoArrayJSX }]
          _ -> [] 
      domSyntaxSafetyReport =
        case config.rules."dom-syntax-safety" of
          Just (PSLint.Config conf)-> [{ status : "error", uri, type: "dom-syntax-safety", ranges : cstMatchModule (lintDomSyntaxSafety conf) }]
          _ -> [] 
      noClassConstraintsReport =
        case config.rules."no-class-constraint" of
          Just PSLint.Error -> [{ status : "error", type: "no-class-constraint", uri, ranges : cstMatchModule lintNoClassConstraints }]
          Just PSLint.Warn -> [{ status : "warn", type: "no-class-constraint", uri, ranges : cstMatchModule lintNoClassConstraints }]
          _ -> []
  in mandatorySignatureReport <> noArrayJSXReport <> domSyntaxSafetyReport <> noClassConstraintsReport 
  where 
  cstMatchModule :: PSLint -> Array (String /\ CST.SourceRange)
  cstMatchModule pslint = 
    case parseModule content of
      ParseSucceeded m -> 
        (pslint.lintModule m) <> 
        (foldMapModule $ 
            { onExpr : pslint.lintExpr
            , onType : pslint.lintType
            , onBinder : pslint.lintBinder
            , onDecl : pslint.lintDecl
            }) m
      ParseSucceededWithErrors _ err -> map (\{error, position} -> (printParseError error) /\ { start : {line : position.line + 1, column : position.column + 1}, end : {line : position.line + 1, column : position.column + 1}}) $ NArray.toArray err
      ParseFailed {error, position} -> [(printParseError error) /\ { start : {line : position.line + 1, column : position.column + 1}, end : {line : position.line + 1, column : position.column + 1}}]