module ClientLSP
  ( ExtensionContext
  , activate
  , deactivate
  )
  where

import Prelude


foreign import data ExtensionContext :: Type
foreign import activate :: ExtensionContext -> Unit
foreign import deactivate :: Unit -> Unit