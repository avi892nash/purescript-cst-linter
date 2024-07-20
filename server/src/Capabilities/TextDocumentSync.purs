module Capabilities.TextDocumentSync where

import Control.Alt
import Prelude

import Capabilities.Diagnostic (Range(..))
import Data.Array (foldl)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import Types (NotificationMessage(..), Request(..))
import Yoga.JSON (class ReadForeign, read, readImpl)


newtype DidChangeTextDocumentParams =
  DidChangeTextDocumentParams 
    { textDocument :: VersionedTextDocumentIdentifier
    , contentChanges :: Array TextDocumentContentChangeEvent
    }

instance ReadForeign DidChangeTextDocumentParams where
  readImpl fgn = DidChangeTextDocumentParams <$> readImpl fgn 

type VersionedTextDocumentIdentifier =
  { version :: Int
  , uri :: String
  }

data TextDocumentContentChangeEvent =
  TextDocumentContentChangeEventA
    { range :: Range
    , rangeLength :: Maybe Int
    , text :: String
    }
  | TextDocumentContentChangeEventB { text :: String }

instance ReadForeign TextDocumentContentChangeEvent where
  readImpl fgn = (TextDocumentContentChangeEventA <$> readImpl fgn) <|> (TextDocumentContentChangeEventB <$> readImpl fgn)

handleChangeTextDoc :: Ref (Map.Map String String) -> NotificationMessage DidChangeTextDocumentParams -> Effect Unit
handleChangeTextDoc refDocMap (NotificationMessage { params : DidChangeTextDocumentParams req }) = do
  docMap <- Ref.read refDocMap
  let content = fromMaybe "" $ Map.lookup req.textDocument.uri docMap
  let newContent = 
        foldl (\b chg -> 
          case chg of
            TextDocumentContentChangeEventA c -> let _ = spy "Avinash" c in b
            TextDocumentContentChangeEventB { text } -> text
          ) content req.contentChanges
  _ <- Ref.modify (Map.insert req.textDocument.uri newContent) refDocMap
  pure unit

