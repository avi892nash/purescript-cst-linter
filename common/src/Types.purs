module PSLint.Types where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe)
import Foreign (ForeignError(..))
import Rules.DomSyntaxSafety (DomSyntaxConfig)
import Yoga.JSON (class ReadForeign, class WriteForeign, read, readImpl, writeImpl)




data RulesConfig a = Error | Warn | Config a

instance ReadForeign a => ReadForeign (RulesConfig a) where
  readImpl fgn = do
    case read fgn :: _ String of
      Right "error" -> pure $ Error
      Right "warn" -> pure $ Warn
      _ -> 
        (Config <$> readImpl fgn)
        <|>
        (throwError $ NEL.singleton $ ForeignError "rules enum are wrong")

instance WriteForeign a => WriteForeign (RulesConfig a) where
  writeImpl Error = writeImpl "error"
  writeImpl Warn = writeImpl "warn"
  writeImpl (Config a) = writeImpl a
  
  

type PSLintConfig 
  = 
  { files :: Array String
  , ignore :: Array String
  , rules :: 
    { "mandatory-signature" :: Maybe (RulesConfig {})
    , "no-array-jsx" :: Maybe (RulesConfig { exprApp :: Maybe (Array String)})
    , "no-class-constraint" :: Maybe (RulesConfig {})
    , "dom-syntax-safety" :: Maybe (RulesConfig DomSyntaxConfig)
    } 
}