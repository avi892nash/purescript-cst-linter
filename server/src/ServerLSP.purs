module ServerLSP where

import Prelude

import Capabilities.Diagnostic (handleDiagnosticRequest)
import Capabilities.Initialize (handleIntializeRequest)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Nullable as N
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Node.EventEmitter as EE
import Node.Process as P
import Types (Response(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, writeImpl)



ipcInputHandler ∷ Effect Unit
ipcInputHandler = do
  EE.on_ P.messageH (\fgn _ -> do
        case read fgn :: _ { method :: String } of
          Right { method } -> do
            Console.log $ "Request: " <> method
            case method of
              "initialize" -> handleResponse (handleIntializeRequest >>> ipcResponseHandler method) fgn
              "textDocument/diagnostic" -> handleResponse (handleDiagnosticRequest >>> ipcResponseHandler method) fgn
              _ -> pure unit
          Left err -> let _ = spy (show err) fgn in pure unit
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
  Console.log $ "Response: " <> method
  pure unit

main ∷ Unit
main = unsafePerformEffect ipcInputHandler 