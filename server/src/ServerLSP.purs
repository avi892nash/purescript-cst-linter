module ServerLSP where

import Prelude

import Capabilities.Diagnostic (handleDiagnosticRequest, makeDiagnosticReport)
import Capabilities.Initialize (handleIntializeRequest, refreshPSLintConfig)
import Capabilities.TextDocumentSync (handleChangeTextDoc, handleDidOpen, handleDidSave)
import Data.Array (foldMap)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as N
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (modify)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Node.EventEmitter as EE
import Node.Process as P
import PSLint (lintAllFiles)
import Types (Diagnostic, DidCloseTextDocumentParams, LintState, Request(..), Response(..), ModuleStatus)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, writeImpl, writeJSON)


ipcInputHandler ∷ LintState -> Effect Unit
ipcInputHandler lintState = do
  EE.on_ P.messageH (\fgn _ -> do
        case read fgn :: _ { method :: String } of
          Right { method } -> do
            -- t <- Ref.read currentDocChanges 
            -- let _ = spy "Avinash" t
            -- let _ = spy "Avinash" fgn
            Console.log $ "Request: " <> method
            case method of
              "initialized" -> do
                config <- Ref.read lintState.psLintConfig
                Console.log $ "Initialized by : " <> writeJSON config
                _ <- launchAff $ readAndLintAllFiles lintState
                pure unit 
                        

              "initialize" -> handleResponse (handleIntializeRequest lintState (ipcResponseHandler method) ipcRequestSend) fgn
              "textDocument/didClose" -> do
                handleResponse 
                  (\({params : p} :: { params :: DidCloseTextDocumentParams}) -> do
                    -- s <- Ref.read lintState.moduleStatus
                    -- let _ = spy "Avinash" $ lookup p.textDocument.uri s
                    -- lintState.clearDiagnosticReport (Just p.textDocument.uri) lintState.moduleStatus
                    lintState.sendDiagnosticReport (Just p.textDocument.uri) lintState.moduleStatus
                  ) fgn
                pure unit
              "textDocument/diagnostic" -> do
                handleResponse (handleDiagnosticRequest lintState (ipcResponseHandler method)) fgn
                pure unit
              "textDocument/didSave" -> do
                handleResponse (handleDidSave lintState) fgn
              "textDocument/didOpen" -> do
                handleResponse (handleDidOpen lintState) fgn 
              "textDocument/didChange" -> do
                handleResponse (handleChangeTextDoc lintState) fgn
              "workspace/didChangeWatchedFiles" -> do
                liftEffect $ lintState.clearDiagnosticReport Nothing lintState.moduleStatus
                refreshPSLintConfig lintState.psLintConfig (readAndLintAllFiles lintState) 
                pure unit
              "shutdown" -> do
                Console.log "Shutting down server..."
                -- Clean up any resources
                handleResponse shutdownResponse fgn
              _ -> pure unit
          Left err -> let _ = spy ("Error in Decoding Request : " <> show err) fgn in pure unit
      ) P.process
  where
    handleResponse :: forall a. ReadForeign a => (a -> Effect Unit) -> Foreign -> Effect Unit
    handleResponse fn fgn = 
      case read fgn of
        Right v -> fn v
        Left err -> Console.log $ show err 

ipcResponseHandler :: forall a. WriteForeign a => String -> Response a -> Effect Unit
ipcResponseHandler method (Response val) = do
  _ <- P.unsafeSend (unsafeCoerce $ writeImpl val) N.null
  Console.log $ "Response: " <> method <> ": " <> writeJSON val
  pure unit

ipcRequestSend :: forall a. WriteForeign a => Request a -> Effect Unit
ipcRequestSend (Request val) = do
  _ <- P.unsafeSend (unsafeCoerce $ writeImpl val) N.null
  Console.log $ "Query from Server: " <> writeJSON val 
  pure unit

shutdownResponse :: Request Int -> Effect Unit
shutdownResponse (Request { id }) = ipcResponseHandler "shutdown" $ Response
                    { jsonrpc: "2.0"
                    , id
                    , result: Nothing :: Maybe Int 
                    , error: Nothing
                    }


readAndLintAllFiles :: LintState -> Aff Unit
readAndLintAllFiles lintState = do 
  config <- liftEffect $ Ref.read lintState.psLintConfig
  ranges <- lintAllFiles config
  _ <- liftEffect $ traverse
    (\{ uri, params, content } -> do
      let diagnostics = foldMap (\p -> makeDiagnosticReport p.status p.ranges) params
      modify 
        (Map.alter 
          (case _ of
            Just o -> Just $ o { diagnostics = diagnostics }
            Nothing -> Just { diagnostics, currentChanges : content }
          ) uri
        ) lintState.moduleStatus
    )
    ranges
  liftEffect $ lintState.clearDiagnosticReport Nothing lintState.moduleStatus
  liftEffect $ lintState.sendDiagnosticReport Nothing lintState.moduleStatus
                        

main ∷ Unit
main = unsafePerformEffect $ do
  psLintConfig <- Ref.new { 
    files : ["src/**/*.purs"], ignore: [], rules : 
    { "mandatory-signature" : Nothing
    , "no-array-jsx" : Nothing
    , "no-class-constraint" : Nothing
    , "dom-syntax-safety" :Nothing
    }
    }
  moduleStatus <- Ref.new Map.empty
  let lintState = {
    psLintConfig,
    moduleStatus,
    sendDiagnosticReport,
    clearDiagnosticReport
  }
  ipcInputHandler lintState
  where
    sendFileDiagnostics :: String -> Array Diagnostic -> Effect Unit
    sendFileDiagnostics uri diagnostics = do
      let params = { uri, diagnostics }
      ipcRequestSend $ Request
        { jsonrpc: "2.0"
        , method: "textDocument/publishDiagnostics"
        , id: Nothing
        , params: params
        }
    sendDiagnosticReport :: Maybe String -> Ref.Ref (Map.Map String ModuleStatus) -> Effect Unit
    sendDiagnosticReport Nothing moduleStatus = do
      currDocChanges <- Ref.read moduleStatus
      _ <- traverse (\(Tuple uri p) -> do
            sendFileDiagnostics uri p.diagnostics
        ) $ (Map.toUnfoldable currDocChanges :: Array (Tuple String ModuleStatus))
      pure unit 
    sendDiagnosticReport (Just uri) moduleStatus = do
      refreshModuleDiagnosticReport uri moduleStatus 

    clearDiagnosticReport :: Maybe String -> Ref.Ref (Map.Map String ModuleStatus) -> Effect Unit
    clearDiagnosticReport Nothing moduleStatus = do
      currDocChanges <- Ref.read moduleStatus
      _ <- traverse (\(Tuple uri _) -> do
            sendFileDiagnostics uri []
        ) $ (Map.toUnfoldable currDocChanges :: Array (Tuple String ModuleStatus))
      pure unit  
    clearDiagnosticReport (Just uri) _ = do
      sendFileDiagnostics uri []

    refreshModuleDiagnosticReport uri moduleStatus = do
      currDocChanges <- Ref.read moduleStatus
      let diagnostic = 
            case lookup uri currDocChanges of
              Just content -> content.diagnostics
              Nothing -> []
      sendFileDiagnostics uri diagnostic 


