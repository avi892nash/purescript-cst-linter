module Capabilities.TextDocumentSync where

import Prelude

import Capabilities.Diagnostic (makeDiagnosticReport)
import Data.Array (foldMap, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Ref as Ref
import PSLint (lintModule)
import Types (DidChangeTextDocumentParams(..), DidOpenTextDocumentParams, DidSaveTextDocumentParams, LintState, NotificationMessage(..), TextDocumentContentChangeEvent(..))

handleChangeTextDoc :: LintState -> NotificationMessage DidChangeTextDocumentParams -> Effect Unit
handleChangeTextDoc { moduleStatus : refDocMap, psLintConfig } (NotificationMessage { params : DidChangeTextDocumentParams req }) = do
  config <- Ref.read psLintConfig 
  docMap <- Ref.read refDocMap
  let content = fromMaybe "" $ _.currentChanges <$> Map.lookup req.textDocument.uri docMap
  let newContent = 
        foldl (\b chg -> 
          case chg of
            TextDocumentContentChangeEventA c -> b
            TextDocumentContentChangeEventB { text } -> text
          ) content req.contentChanges
  _ <- Ref.modify (Map.insert req.textDocument.uri { currentChanges : newContent, diagnostics : foldMap (\p -> makeDiagnosticReport p.status p.ranges) $ lintModule config req.textDocument.uri newContent}) refDocMap
  pure unit


handleDidSave :: LintState -> NotificationMessage DidSaveTextDocumentParams -> Effect Unit
handleDidSave { moduleStatus : refDocMap, psLintConfig } (NotificationMessage { params : {textDocument, text} }) = do
  config <- Ref.read psLintConfig 
  case text of
    Just val -> void $ Ref.modify (Map.insert textDocument.uri { currentChanges : val, diagnostics : foldMap (\p -> makeDiagnosticReport p.status p.ranges) $ lintModule config textDocument.uri val}) refDocMap 
    Nothing -> pure unit


handleDidOpen :: LintState -> NotificationMessage DidOpenTextDocumentParams -> Effect Unit
handleDidOpen { moduleStatus : refDocMap, psLintConfig } (NotificationMessage { params : { textDocument : {uri, text} }}) = do 
  config <- Ref.read psLintConfig
  void $ Ref.modify (Map.insert uri { currentChanges : text, diagnostics : foldMap (\p -> makeDiagnosticReport p.status p.ranges) $ lintModule config uri text }) refDocMap
