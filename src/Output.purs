module Output where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable

import Types (Board, Move, Column)

printBoard :: forall e. Board Move -> Eff (console :: CONSOLE | e) Unit
printBoard board =
  traverse_ (log <<< printCol) board

printCol :: Column Move -> String
printCol col =
  foldMap show col

