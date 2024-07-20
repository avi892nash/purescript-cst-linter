module Rules.NoClassConstraints where


import Prelude
import Data.Array (length)
import Data.Tuple.Nested ((/\))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapType)
import PureScript.CST.Types as CST
import Utils (PSLint, defaultLinter)

lintNoClassConstraints :: PSLint
lintNoClassConstraints = 
  defaultLinter 
    { lintDecl = 
        case _ of
          e@(CST.DeclSignature (CST.Labeled { value })) -> Prelude.do
            let s = foldMapType (defaultMonoidalVisitor
                              { onType = 
                                  case _ of
                                    CST.TypeConstrained _ _ _ -> [1]
                                    _ -> []
                              }) value
            if length s > 0 
              then ["Class Constraint are not allowed" /\ (rangeOf e)]
              else []
          _ -> []
    }