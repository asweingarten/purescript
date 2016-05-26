module Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, print)
import Control.Monad.State.Trans
import Control.Monad.State.Class

import Data.Generic

import Data.List

type UI e = Eff (console :: CONSOLE | e)
type Game e = StateT (Board Move) (UI e)

type Board a = List (Column a)

type Column a = List a
data Move = Red | Black
derive instance genericMove :: Generic Move

instance showMove :: Show Move where
  show Red = " Red "
  show Black = "Black"

instance eqMove :: Eq Move where
  eq = gEq
