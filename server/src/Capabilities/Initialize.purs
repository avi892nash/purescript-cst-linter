module Capabilities.Initialize where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, modify, write)
import Node.Buffer (toString)
import Node.Encoding as Encoding
import Node.FS.Sync (readFile)
import PSLint.Types (PSLintConfig)
import Types (DiagnosticProvider(..), InitializeParams(..), InitializeResult(..), Request(..), Response(..), ServerCapabilities(..), TextDocumentSync(..), TextDocumentSyncKind(..), LintState)
import Version (version)
import Yoga.JSON (class WriteForeign, readJSON)


handleIntializeRequest :: LintState -> (forall a. WriteForeign a => Response a -> Effect Unit) -> (forall b. WriteForeign b => Request b -> Effect Unit) -> Request InitializeParams -> Effect Unit
handleIntializeRequest refpsLintConfig callbackResp ipcRequestSend (Request {id, params : InitializeParams _})  = do
  mbSpagoDhall <- try $ toString Encoding.UTF8 =<< readFile "./spago.dhall"
  eiContent <- try $ toString Encoding.UTF8 =<< readFile "./.pslintrc"
  case mbSpagoDhall, eiContent of
    Right _, Right content -> 
      case readJSON content :: _ PSLintConfig of
        Right psLintConfig -> do
            write psLintConfig refpsLintConfig.psLintConfig
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


