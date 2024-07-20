module Rules.NoClassConstraints where



import PureScript.CST.Types as CST


import Utils (PSLint, defaultLinter)

lintNoClassConstraints :: PSLint
lintNoClassConstraints = 
  defaultLinter 
    { lintDecl = 
        case _ of
          CST.DeclSignature _ -> []
          _ -> []
    }