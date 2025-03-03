module Types where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, ForeignError(..), fail)
import PSLint.Types (PSLintConfig)
import PureScript.CST.Types as CST
import Record (merge)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, readImpl, writeImpl)

type TextDocumentIdentifier r = { uri :: String | r }

type PartialResultParams r = ( partialResultToken :: Maybe ProgressToken | r )

type WorkDoneProgressParams r = ( workDoneToken :: Maybe ProgressToken | r )

type WorkDoneProgressOptions r = ( workDoneProgress :: Maybe Boolean | r )

type ClientCapabilities = {}

type Message r = (jsonrpc :: String | r)

newtype NotificationMessage a = NotificationMessage (Record (Message (method :: String, params :: a)))

instance ReadForeign a => ReadForeign (NotificationMessage a) where
  readImpl fgn = NotificationMessage <$> readImpl fgn 

newtype Request a = Request (Record (Message (id :: Maybe Int, method :: String, params :: a)))

instance ReadForeign a => ReadForeign (Request a) where
  readImpl fgn = Request <$> readImpl fgn 

newtype Response b = Response (Record (Message (id :: Maybe Int, result :: b, error :: Maybe ResponseError)))

instance WriteForeign b => WriteForeign (Response b) where
  writeImpl (Response r) = writeImpl r

newtype ResponseError = ResponseError { code :: Int, message :: String, data :: Maybe Foreign }

instance WriteForeign ResponseError where
  writeImpl (ResponseError r) = writeImpl r

data ErrorCodes
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError

instance WriteForeign ErrorCodes where
  writeImpl err =
    case err of
      ParseError -> writeImpl (-32700)
      InvalidRequest -> writeImpl (-32600)
      MethodNotFound -> writeImpl (-32601)
      InvalidParams -> writeImpl (-32602)
      InternalError -> writeImpl (-32603)

data Encoding = UTF8 | UTF16 | UTF32

instance WriteForeign Encoding where
  writeImpl en =
    case en of
      UTF8 -> writeImpl "utf-8"
      UTF16 -> writeImpl "utf-16"
      UTF32 -> writeImpl "utf-32"


data TextDocumentSyncKind
  = None
  | Full
  | Incremental

instance WriteForeign TextDocumentSyncKind where
  writeImpl None = writeImpl 0
  writeImpl Full = writeImpl 1
  writeImpl Incremental = writeImpl 2

data TextDocumentSync
  = TextDocumentSyncOptions { openClose :: Maybe Boolean, change :: Maybe TextDocumentSyncKind, save :: { includeText :: Maybe Boolean } }
  | TextDocumentSyncKind TextDocumentSyncKind

instance WriteForeign TextDocumentSync where
  writeImpl (TextDocumentSyncKind e) = writeImpl e
  writeImpl (TextDocumentSyncOptions o) = writeImpl o

type StaticRegistrationOptions r = ( id :: Maybe String | r )
type DiagnosticOptions r = (identifier :: Maybe String, interFileDependencies :: Boolean, workspaceDiagnostics :: Boolean | r)
type TextDocumentRegistrationOptions r = { documentSelector :: Maybe DocumentSelector | r }
type DocumentSelector = Array { language :: Maybe String, scheme :: Maybe String, pattern :: Maybe String }

data DiagnosticProvider
  = DiagnosticOptions (Record (WorkDoneProgressOptions (identifier :: Maybe String, interFileDependencies :: Boolean, workspaceDiagnostics :: Boolean)))
  | DiagnosticRegistrationOptions (TextDocumentRegistrationOptions (DiagnosticOptions (StaticRegistrationOptions ())))

instance WriteForeign DiagnosticProvider where
  writeImpl (DiagnosticOptions o) = writeImpl o
  writeImpl (DiagnosticRegistrationOptions o) = writeImpl o


data TraceValue = OFF | MESSAGE | VERBOSE 
instance WriteForeign TraceValue where
  writeImpl OFF = writeImpl "off"
  writeImpl MESSAGE = writeImpl "message"
  writeImpl VERBOSE = writeImpl "verbose"

instance ReadForeign TraceValue where
  readImpl fgn =
    case read fgn :: _ String of
      Right "off" -> pure OFF
      Right "message" -> pure MESSAGE
      Right "verbose" -> pure VERBOSE
      Right _ -> fail $ ForeignError "Trace Value enum is not correct"
      Left err -> throwError err

newtype WorkspaceFolder = WorkspaceFolder { uri :: String , name :: String }

instance WriteForeign WorkspaceFolder where
  writeImpl (WorkspaceFolder g) = writeImpl g
instance ReadForeign WorkspaceFolder where
  readImpl fgn = WorkspaceFolder <$> readImpl fgn

type ProgressToken = StringOrInt

data StringOrInt = String String | Int Int

instance ReadForeign StringOrInt where
  readImpl fgn = (String <$> readImpl fgn) <|> (Int <$> readImpl fgn)

instance WriteForeign StringOrInt where
  writeImpl (String str) = writeImpl str
  writeImpl (Int val) = writeImpl val

type ShowMessageParams = {
  type :: Int,
  message :: String
}

type ModuleStatus = { currentChanges :: String, diagnostics :: Array Diagnostic }

type LintState = {
  psLintConfig :: Ref PSLintConfig,
  moduleStatus :: Ref (Map.Map String ModuleStatus),
  refreshDiagnosticReport :: Maybe String -> Ref.Ref (Map.Map String ModuleStatus) -> Effect Unit,
  clearDiagnosticReport :: String -> Effect Unit 
}



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




newtype DidChangeTextDocumentParams =
  DidChangeTextDocumentParams 
    { textDocument :: VersionedTextDocumentIdentifier
    , contentChanges :: Array TextDocumentContentChangeEvent
    }

instance ReadForeign DidChangeTextDocumentParams where
  readImpl fgn = DidChangeTextDocumentParams <$> readImpl fgn 

type VersionedTextDocumentIdentifier =
  { version :: Int
  , uri :: String
  }

data TextDocumentContentChangeEvent =
  TextDocumentContentChangeEventA
    { range :: Range
    , rangeLength :: Maybe Int
    , text :: String
    }
  | TextDocumentContentChangeEventB { text :: String }

instance ReadForeign TextDocumentContentChangeEvent where
  readImpl fgn = (TextDocumentContentChangeEventA <$> readImpl fgn) <|> (TextDocumentContentChangeEventB <$> readImpl fgn)



type DidOpenTextDocumentParams = { textDocument :: TextDocumentItem }
type TextDocumentItem = { uri :: String, languageId :: String, version :: Int, text :: String }

type DidSaveTextDocumentParams = { textDocument :: TextDocumentIdentifier (), text :: Maybe String }


newtype InitializeParams
  = InitializeParams 
      (Record (WorkDoneProgressOptions
        ( processId :: Maybe Int
        , clientInfo :: Maybe { name :: String, version :: Maybe String }
        , locale :: Maybe String
        , rootPath :: Maybe String
        , rootUri :: Maybe String
        , initializationOptions :: Maybe Foreign
        , capabilities :: ClientCapabilities
        , trace :: Maybe TraceValue
        , workspaceFolders :: Maybe (Array WorkspaceFolder)
        )
      ))

instance ReadForeign InitializeParams where
  readImpl fgn = InitializeParams <$> readImpl fgn

newtype InitializeResult = InitializeResult {
  capabilities :: ServerCapabilities,
  serverInfo :: Maybe { name :: String, version :: Maybe String }
}
instance WriteForeign InitializeResult where
  writeImpl (InitializeResult res) = writeImpl res


data ServerCapabilities
  = ServerCapabilities 
  { positionEncoding :: Maybe Encoding
  , textDocumentSync :: Maybe TextDocumentSync
  , diagnosticProvider :: Maybe DiagnosticProvider
  , window :: Maybe { showMessage :: Maybe { messageActionItem :: Maybe { additionalPropertiesSupport :: Maybe Boolean }}}
  , executeCommandProvider :: Maybe (Record (WorkDoneProgressOptions(commands :: Array String )))
  }

instance WriteForeign ServerCapabilities where
  writeImpl (ServerCapabilities p) = writeImpl p