module Types where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Foreign (Foreign, ForeignError(..), fail)
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