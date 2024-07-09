module Capabilities.Initialize where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Types (ClientCapabilities, DiagnosticProvider(..), Encoding, Request(..), Response(..), TextDocumentSync(..), TextDocumentSyncKind(..), TraceValue, WorkDoneProgressOptions, WorkspaceFolder)
import Version (version)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

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


data ServerCapabilities = ServerCapabilities {
  positionEncoding :: Maybe Encoding
, textDocumentSync :: Maybe TextDocumentSync
, diagnosticProvider :: Maybe DiagnosticProvider
}

instance WriteForeign ServerCapabilities where
  writeImpl (ServerCapabilities p) = writeImpl p

handleIntializeRequest :: Request InitializeParams -> Response InitializeResult
handleIntializeRequest (Request {id, params : InitializeParams _}) = 
  Response 
    { jsonrpc : "2.0"
    , id : Just id
    , result : 
        InitializeResult 
          { capabilities : 
              ServerCapabilities 
                { positionEncoding : Nothing
                , diagnosticProvider : Just $ DiagnosticOptions { identifier : Nothing, interFileDependencies : false, workspaceDiagnostics : false, workDoneProgress : Nothing }
                , textDocumentSync : Just $ TextDocumentSyncKind Full }
          , serverInfo : Just { name : "pslint-server", version : Just version }
          }
    , error : Nothing }