module Capabilities.Diagnostic where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Foreign (Foreign, ForeignError(..), fail)
import PureScript.CST.Types as CST
import Record (merge)
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


handleDiagnosticRequest :: Request DocumentDiagnosticParams -> Response DocumentDiagnosticReport
handleDiagnosticRequest (Request {id, params : p}) = 
  let _ = spy "Avinash" p in
  Response 
    { jsonrpc : "2.0"
    , id : Just id
    , error : Nothing
    , result : RelatedFullDocumentDiagnosticReport { relatedDocuments : Nothing, resultId : Nothing, items : [Diagnostic { data : Nothing, relatedInformation : Nothing, tags : Nothing, message : "Error came", source : Just "PSLint", codeDescription : Nothing, code : Nothing, severity : Just Error, range : Range {start : {line : 1, column : 1}, end : {line : 1, column : 10}} }]} 
    }