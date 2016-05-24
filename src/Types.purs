module Types where

import Prelude

type Board = Array Column
type Column = Array Space
data Space = Red | Black

instance showSpace :: Show Space where
  show Red = " Red "
  show Black = "Black"
