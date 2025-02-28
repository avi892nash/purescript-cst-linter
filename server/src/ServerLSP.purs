module ServerLSP where

import Prelude

import Capabilities.Diagnostic (handleDiagnosticRequest)
import Capabilities.Initialize (handleIntializeRequest)
import Capabilities.TextDocumentSync (handleChangeTextDoc, handleDidOpen, handleDidSave)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as N
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Node.EventEmitter as EE
import Node.Process as P
import PSLint.Types (PSLintConfig)
import Types (Request(..), Response(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, writeImpl, writeJSON)


ipcInputHandler ∷ Ref PSLintConfig -> Ref (Map.Map String String) -> Effect Unit
ipcInputHandler psLintConfig currentDocChanges = do
  EE.on_ P.messageH (\fgn _ -> do
        case read fgn :: _ { method :: String } of
          Right { method } -> do
            -- t <- Ref.read currentDocChanges 
            -- let _ = spy "Avinash" t
            -- let _ = spy "Avinash" fgn
            Console.log $ "Request: " <> method
            case method of
              "initialized" -> do
                config <- Ref.read psLintConfig
                Console.log $ "Initialized by : " <> writeJSON config 
              "initialize" -> handleResponse (handleIntializeRequest psLintConfig (ipcResponseHandler method) ipcRequestSend) fgn
              "textDocument/diagnostic" -> do
                config <- Ref.read psLintConfig 
                handleResponse (handleDiagnosticRequest config currentDocChanges >=> ipcResponseHandler method) fgn
                pure unit
              "textDocument/didSave" -> do
                handleResponse (handleDidSave currentDocChanges) fgn
              "textDocument/didOpen" -> do
                handleResponse (handleDidOpen currentDocChanges) fgn 
              "textDocument/didChange" -> do
                handleResponse (handleChangeTextDoc currentDocChanges) fgn
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

main ∷ Unit
main = unsafePerformEffect $ do
  psLintConfig <- Ref.new { files : ["src/**/*.purs"], ignore: [], rules : 
    { "mandatory-signature" : Nothing
    , "no-array-jsx" : Nothing
    , "no-class-constraint" : Nothing
    , "dom-syntax-safety" :Nothing
    } }
  currentDocChanges <- Ref.new Map.empty
  ipcInputHandler psLintConfig currentDocChanges