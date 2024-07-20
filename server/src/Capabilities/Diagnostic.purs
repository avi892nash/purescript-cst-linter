module Capabilities.Diagnostic where

import Prelude

import Data.Array (fold, foldMap, foldl)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, ForeignError(..), fail)
import PSLint (lint, lintModule)
import PSLint.Types as PSLint
import PureScript.CST.Types as CST
import Record (merge)
import Rules.DomSyntaxSafety as R
import Rules.MandatorySignature as R
import Rules.NoArrayJSX as R
import Rules.NoClassConstraints as R
import Types (PartialResultParams, Request(..), Response(..), StringOrInt, TextDocumentIdentifier, WorkDoneProgressParams)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, readImpl, writeImpl)



newtype DocumentDiagnosticParams 
  = DocumentDiagnosticParams 
      ( Record 
          (PartialResultParams
          (WorkDoneProgressParams
            (identifier :: Maybe String, previousResultId :: Maybe String, textDocument :: TextDocumentIdentifier ())
          )
        )
      )

instance ReadForeign DocumentDiagnosticParams where
  readImpl fgn = DocumentDiagnosticParams <$> (readImpl fgn)

data DocumentDiagnosticReport
  = RelatedFullDocumentDiagnosticReport { resultId :: Maybe String, items :: Array Diagnostic, relatedDocuments :: Maybe (Array Foreign)}
  | RelatedUnchangedDocumentDiagnosticReport { resultId :: String, relatedDocuments :: Maybe (Array Foreign)}

instance WriteForeign DocumentDiagnosticReport where
  writeImpl (RelatedFullDocumentDiagnosticReport val) = writeImpl (merge val { kind : "full"})
  writeImpl (RelatedUnchangedDocumentDiagnosticReport val) = writeImpl (merge val { kind : "unchanged"}) 


data DiagnosticSeverity = Error | Warning | Information | Hint

instance WriteForeign DiagnosticSeverity where
  writeImpl Error = writeImpl 1
  writeImpl Warning = writeImpl 2
  writeImpl Information = writeImpl 3
  writeImpl Hint = writeImpl 4

instance ReadForeign DiagnosticSeverity where
  readImpl fgn =
    case read fgn :: _ Int of
      Right 1 -> pure Error
      Right 2 -> pure Warning
      Right 3 -> pure Information
      Right 4 -> pure Hint
      _ -> fail $ ForeignError "Integer value does not map to DiagnosticSeverity"

data DiagnosticTag = Unnecessary | Deprecated


instance WriteForeign DiagnosticTag where
  writeImpl Unnecessary = writeImpl 1
  writeImpl Deprecated = writeImpl 2

instance ReadForeign DiagnosticTag where
  readImpl fgn =
    case read fgn :: _ Int of
      Right 1 -> pure Unnecessary
      Right 2 -> pure Deprecated
      _ -> fail $ ForeignError "Integer value does not map to DiagnosticTag"

newtype Range = Range CST.SourceRange

instance ReadForeign Range where
  readImpl fgn = do
    p <- readImpl fgn :: _ { start :: { line :: Int, character :: Int }, end :: { line :: Int, character :: Int } }
    pure $ Range { start : { line : p.start.line, column : p.start.character }, end : { line : p.end.line, column : p.end.character } }

instance WriteForeign Range where
  writeImpl (Range val) = writeImpl { start : { line : val.start.line, character : val.start.column}, end : { line : val.end.line, character : val.end.column}}

newtype Diagnostic
  = Diagnostic 
      { range :: Range 
      , severity :: Maybe DiagnosticSeverity 
      , code :: Maybe StringOrInt
      , codeDescription :: Maybe { href :: String }
      , source :: Maybe String
      , message :: String
      , tags :: Maybe DiagnosticTag
      , relatedInformation :: Maybe (Array { location :: { uri :: String, range :: Range }, message :: String })
      , data :: Maybe Foreign
      }

instance WriteForeign Diagnostic where
  writeImpl (Diagnostic val) = writeImpl val


handleDiagnosticRequest :: PSLint.PSLintConfig -> Ref (Map.Map String String) -> Request DocumentDiagnosticParams -> Effect (Response DocumentDiagnosticReport)
handleDiagnosticRequest config refCurrentDocChanges (Request {id, params : (DocumentDiagnosticParams p) }) = do
  currentTextChange <- Ref.read refCurrentDocChanges
  let mbContent = Map.lookup p.textDocument.uri currentTextChange
      mandatorySignatureReport =
        case config.rules."mandatory-signature" of
          Just PSLint.Error -> makeDiagnosticErrorReport <<< lintModule R.lintMandatorySignature
          Just PSLint.Warn  -> makeDiagnosticWarnReport <<< lintModule R.lintMandatorySignature
          _ -> \_ -> []
      noArrayJSXReport =
        case config.rules."no-array-jsx" of
          Just PSLint.Error -> makeDiagnosticErrorReport <<< lintModule R.lintNoArrayJSX
          Just PSLint.Warn  -> makeDiagnosticWarnReport <<< lintModule R.lintNoArrayJSX
          _ -> \_ -> [] 
      domSyntaxSafetyReport =
        case config.rules."dom-syntax-safety" of
          Just (PSLint.Config conf)-> makeDiagnosticErrorReport <<< lintModule (R.lintDomSyntaxSafety conf)
          _ -> \_ -> [] 
      noClassConstraintsReport =
        case config.rules."no-class-constraint" of
          Just PSLint.Error -> makeDiagnosticErrorReport <<< lintModule R.lintNoClassConstraints 
          Just PSLint.Warn -> makeDiagnosticWarnReport <<< lintModule R.lintNoClassConstraints 
          _ -> \_ -> []
      
  let diagnosticGenerator = \content -> foldMap (\fn -> fn content) [mandatorySignatureReport, noArrayJSXReport, domSyntaxSafetyReport, noClassConstraintsReport] 
  
  let response = maybe [] diagnosticGenerator mbContent

  pure $ Response 
      { jsonrpc : "2.0"
      , id : Just id
      , error : Nothing
      , result : RelatedFullDocumentDiagnosticReport { relatedDocuments : Nothing, resultId : Nothing, items : response } 
      }


makeDiagnosticWarnReport :: Array (String /\ CST.SourceRange) -> Array Diagnostic
makeDiagnosticWarnReport arr =
        map
        (\(errorMsg /\ range) -> 
          Diagnostic 
            { data : Nothing
            , relatedInformation : Nothing
            , tags : Nothing
            , message : errorMsg
            , source : Just "PSLint"
            , codeDescription : Nothing
            , code : Nothing
            , severity : Just Warning
            , range : Range range
            }
        )
        arr

makeDiagnosticErrorReport :: Array (String /\ CST.SourceRange) -> Array Diagnostic
makeDiagnosticErrorReport arr =
        map
        (\(errorMsg /\ range) -> 
          Diagnostic 
            { data : Nothing
            , relatedInformation : Nothing
            , tags : Nothing
            , message : errorMsg
            , source : Just "PSLint"
            , codeDescription : Nothing
            , code : Nothing
            , severity : Just Error
            , range : Range range
            }
        )
        arr