module Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, print)
import Control.Monad.State.Trans
import Control.Monad.State.Class

type Board = Array Column
type Column = Array Space
data Space = Red | Black

type UI e = Eff (console :: CONSOLE | e)
type Game e = StateT Board (UI e)

instance showSpace :: Show Space where
  show Red = " Red "
  show Black = "Black"
