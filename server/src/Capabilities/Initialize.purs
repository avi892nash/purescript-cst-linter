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
import Types (ClientCapabilities, DiagnosticProvider(..), Encoding, Request(..), Response(..), ResponseError(..), TextDocumentSync(..), TextDocumentSyncKind(..), TraceValue, WorkspaceFolder, WorkDoneProgressOptions)
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
  , window :: Maybe { showMessage :: Maybe { messageActionItem :: Maybe { additionalPropertiesSupport :: Maybe Boolean }}}
  , executeCommandProvider :: Maybe (Record (WorkDoneProgressOptions(commands :: Array String )))
  }

instance WriteForeign ServerCapabilities where
  writeImpl (ServerCapabilities p) = writeImpl p

handleIntializeRequest :: Ref PSLintConfig -> (forall a. WriteForeign a => Response a -> Effect Unit) -> (forall b. WriteForeign b => Request b -> Effect Unit) -> Request InitializeParams -> Effect Unit
handleIntializeRequest refpsLintConfig callbackResp ipcRequestSend (Request {id, params : InitializeParams _})  = do
  mbSpagoDhall <- try $ toString Encoding.UTF8 =<< readFile "./spago.dhall"
  eiContent <- try $ toString Encoding.UTF8 =<< readFile "./.pslintrc"
  case mbSpagoDhall, eiContent of
    Right _, Right content -> 
      case readJSON content :: _ PSLintConfig of
        Right psLintConfig -> do
            write psLintConfig refpsLintConfig 
            callbackResp serverInfo 
        Left err -> do
          callbackResp serverInfo
          ipcRequestSend $ wrongDirectory $ "Error: eslintrc parsing failed. Write a proper json. => " <> show err
    Left _, _ -> do
      callbackResp serverInfo
      ipcRequestSend $ wrongDirectory "Error: The specified directory does not contain spago.dhall file"
    _, Left _ -> do
      callbackResp serverInfo
      ipcRequestSend $ wrongDirectory "Error: The specified directory does not contain .pslintrc file"
  where
    wrongDirectory message = Request
      { jsonrpc: "2.0"
      , method: "window/showMessage"
      , id : Nothing
      , params : 
          { "type": 1 
          , "message": message -- 
          }
      }
    serverInfo = Response 
              { jsonrpc : "2.0"
              , id : id
              , result : 
                  Just $ InitializeResult 
                    { capabilities : 
                        ServerCapabilities 
                          { positionEncoding : Nothing
                          , diagnosticProvider : Just $ DiagnosticOptions { identifier : Nothing, interFileDependencies : false, workspaceDiagnostics : false, workDoneProgress : Nothing }
                          , textDocumentSync : Just $ TextDocumentSyncOptions { openClose : Just true, change : Just Full, save : { includeText : Just true }}
                          , window : Nothing
                          , executeCommandProvider: Nothing
                          }
                    , serverInfo : Just { name : "pslint-server", version : Just version }
                    }
              , error : Nothing }
