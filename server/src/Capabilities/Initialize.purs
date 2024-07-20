module Capabilities.Initialize where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, write)
import Foreign (Foreign)
import Node.Buffer (toString)
import Node.Encoding as Encoding
import Node.FS.Sync (readFile)
import PSLint.Types (PSLintConfig)
import Types (ClientCapabilities, DiagnosticProvider(..), Encoding, Request(..), Response(..), ResponseError(..), TextDocumentSync(..), TextDocumentSyncKind(..), TraceValue, WorkDoneProgressOptions, WorkspaceFolder)
import Version (version)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, readJSON, writeImpl)

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
  }

instance WriteForeign ServerCapabilities where
  writeImpl (ServerCapabilities p) = writeImpl p

handleIntializeRequest :: Ref PSLintConfig -> Request InitializeParams -> Effect (Response InitializeResult)
handleIntializeRequest refpsLintConfig (Request {id, params : InitializeParams _}) = do
  eiContent <- try $ toString Encoding.UTF8 =<< readFile "./.pslintrc"
  case eiContent of
    Right content -> 
      case readJSON content :: _ PSLintConfig of
        Right psLintConfig -> do
            write psLintConfig refpsLintConfig 
            pure $ Response 
              { jsonrpc : "2.0"
              , id : Just id
              , result : 
                  InitializeResult 
                    { capabilities : 
                        ServerCapabilities 
                          { positionEncoding : Nothing
                          , diagnosticProvider : Just $ DiagnosticOptions { identifier : Nothing, interFileDependencies : false, workspaceDiagnostics : false, workDoneProgress : Nothing }
                          , textDocumentSync : Just $ TextDocumentSyncOptions { openClose : Just true, change : Just Full, save : { includeText : Just true }} }
                    , serverInfo : Just { name : "pslint-server", version : Just version }
                    }
              , error : Nothing }
        Left err -> pure $ error $ ResponseError { code : -32700, message : "Cannot parse .pslintrc file : " <> show err , data : Nothing}
    Left err -> pure $ error $ ResponseError { code : -32700, message : "Cannot find and read .eslintrc file : " <> show err , data : Nothing }
  where
    error msg = 
      Response 
        { jsonrpc : "2.0"
        , error : Just msg
        , id : Just id
        , result : InitializeResult { capabilities : ServerCapabilities { positionEncoding : Nothing, diagnosticProvider : Nothing, textDocumentSync : Nothing }
                    , serverInfo : Just { name : "pslint-server", version : Just version }}
        } 
