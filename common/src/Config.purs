module Config where

import Control.Alt
import Control.Monad.Except
import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty as NEL
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, readImpl)

data RulesConfig a = Error | Warn | Config a

instance ReadForeign a => ReadForeign (RulesConfig a) where
  readImpl fgn = do
    (Config <$> readImpl fgn) 
    <|> 
    (do
      v <- readImpl fgn :: _ String
      case v of
        "error" -> pure $ Error
        "warn" -> pure $ Warn
        _ -> throwError $ NEL.singleton $ ForeignError "rules enum are wrong"
    )
  

type PSLintConfig 
  = 
  { files :: Array String
  , ignore :: Array String
  , rules :: 
    { "mandatory-signature" :: RulesConfig {}
    , "no-array-jsx" :: RulesConfig {}
    , "no-class-containt" :: RulesConfig {}
    } 
  }