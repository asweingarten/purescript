module ReadLine where

import Prelude
import Types (UI)

foreign import readLine :: forall e. UI e String
