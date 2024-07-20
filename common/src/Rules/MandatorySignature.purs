module Rules.MandatorySignature where

import Prelude

import Data.Array (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types as CST
import Utils (PSLint, defaultLinter)


lintMandatorySignature :: PSLint
lintMandatorySignature = 
  defaultLinter {
    lintModule = 
      case _ of
        CST.Module { body : CST.ModuleBody { decls } } -> 
          snd $ foldl (\curr d-> 
            case curr of
              Tuple Nothing r -> 
                case d of
                  CST.DeclSignature _ -> Tuple (Just d) r
                  CST.DeclKindSignature _ _ -> Tuple (Just d) r
                  _ -> Tuple Nothing (r <> ["Declaration is not defined" /\ (rangeOf d)])
              Tuple (Just _) r ->
                case d of
                  CST.DeclSignature _ -> Tuple Nothing r
                  CST.DeclKindSignature _ _ -> Tuple Nothing r
                  _ -> Tuple Nothing r
            ) (Tuple Nothing []) decls
  } 