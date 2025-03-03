module Capabilities.Diagnostic where

import Prelude

import Data.Array (foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Ref as Ref
import PSLint (lintModule)
import PureScript.CST.Types as CST
import Types (Diagnostic(..), DiagnosticSeverity(..), DocumentDiagnosticParams(..), DocumentDiagnosticReport(..), Range(..), Request(..), Response(..), LintState)
import Yoga.JSON (class WriteForeign)


handleDiagnosticRequest :: LintState -> (forall a. WriteForeign a => Response a -> Effect Unit) -> Request DocumentDiagnosticParams -> Effect Unit
handleDiagnosticRequest { psLintConfig , moduleStatus : refCurrentDocChanges, clearDiagnosticReport } callbackResponse (Request {id, params : (DocumentDiagnosticParams p)}) = do
  currentTextChange <- Ref.read refCurrentDocChanges
  config <- Ref.read psLintConfig
  let mbContent = Map.lookup p.textDocument.uri currentTextChange
  let response = 
        case mbContent of
          Just content -> foldMap (\p -> makeDiagnosticReport p.status p.ranges) $ lintModule config p.textDocument.uri content.currentChanges
          Nothing -> []
  
  clearDiagnosticReport p.textDocument.uri
  callbackResponse $ Response 
      { jsonrpc : "2.0"
      , id : id
      , error : Nothing
      , result : RelatedFullDocumentDiagnosticReport { relatedDocuments : Nothing, resultId : Nothing, items : response } 
      }


makeDiagnosticReport :: String -> Array (String /\ CST.SourceRange) -> Array Diagnostic
makeDiagnosticReport "warn" = makeDiagnosticWarnReport
makeDiagnosticReport "error" = makeDiagnosticErrorReport
makeDiagnosticReport _ = \_ -> []

makeDiagnosticWarnReport :: Array (String /\ CST.SourceRange) -> Array Diagnostic
makeDiagnosticWarnReport arr =
        map
        (\(errorMsg /\ range) -> 
          Diagnostic 
            { data : Nothing
            , relatedInformation : Nothing
            , tags : Nothing
            , message : errorMsg
            , source : Just "PSLint"
            , codeDescription : Nothing
            , code : Nothing
            , severity : Just Warning
            , range : Range range
            }
        )
        arr

makeDiagnosticErrorReport :: Array (String /\ CST.SourceRange) -> Array Diagnostic
makeDiagnosticErrorReport arr =
        map
        (\(errorMsg /\ range) -> 
          Diagnostic 
            { data : Nothing
            , relatedInformation : Nothing
            , tags : Nothing
            , message : errorMsg
            , source : Just "PSLint"
            , codeDescription : Nothing
            , code : Nothing
            , severity : Just Error
            , range : Range range
            }
        )
        arr