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
import PSLint (lintModule)
import PSLint.Types as PSLint
import PureScript.CST.Types as CST
import Record (merge)
import Rules.DomSyntaxSafety as R
import Rules.MandatorySignature as R
import Rules.NoArrayJSX as R
import Rules.NoClassConstraints as R
import Types (PartialResultParams, Request(..), Response(..), StringOrInt, TextDocumentIdentifier, WorkDoneProgressParams)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, readImpl, writeImpl)


type DidCloseTextDocumentParams = {
  textDocument :: TextDocumentIdentifier () 
}

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


handleDiagnosticRequest :: PSLint.PSLintConfig -> Ref (Map.Map String String) -> (forall a. WriteForeign a => Response a -> Effect Unit) -> (String -> Effect Unit) -> Request DocumentDiagnosticParams -> Effect Unit
handleDiagnosticRequest config refCurrentDocChanges callbackResponse clearDiagonostic (Request {id, params : (DocumentDiagnosticParams p)}) = do
  currentTextChange <- Ref.read refCurrentDocChanges
  let mbContent = Map.lookup p.textDocument.uri currentTextChange
  
  let response = 
        case mbContent of
          Just content -> 
            foldMap 
              (\{ status, ranges } -> 
                case status of
                  "error" -> makeDiagnosticErrorReport ranges
                  "warn" -> makeDiagnosticWarnReport ranges
                  _ -> []
              ) $
              lintModule config p.textDocument.uri content 
          Nothing -> []
  -- To clear diagonostic from allFiles 
  clearDiagonostic p.textDocument.uri 

  callbackResponse $ Response 
      { jsonrpc : "2.0"
      , id : id
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