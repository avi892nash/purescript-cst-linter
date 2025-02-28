module Rules.MandatorySignature where

import Prelude

import Data.Array (elem, foldl, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Print (print)
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types as CST
import Utils (PSLint, defaultLinter)


lintMandatorySignature :: PSLint
lintMandatorySignature = 
  defaultLinter {
    lintModule = 
      case _ of
        CST.Module { body : CST.ModuleBody { decls } } -> do
          let vals = 
                mapMaybe 
                  (\d ->
                    case d of
                      CST.DeclValue { name : CST.Name { name } } -> Just $ Tuple (trim $ print name) d
                      _ -> Nothing
                  )
                  decls
              typeVals = 
                mapMaybe 
                  (case _ of
                    CST.DeclSignature (CST.Labeled { label : CST.Name { name : label} }) ->  Just (trim $ print label)
                    _ -> Nothing
                  )
                  decls  
          -- let _ = spy "DECLS" $ writeJSON (map (\(Tuple a _) -> a)vals)
          -- let _ = spy "SIGN" $ writeJSON typeVals
          foldl 
            (\b (Tuple name decl) -> 
              if elem name typeVals 
                then  b 
                else b <> [ "Signature is not defined " /\ (rangeOf decl)]
            ) 
            []
            vals
  } 