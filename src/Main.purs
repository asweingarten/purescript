module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, print)

import Data.Array
import Data.Foldable
import Data.Maybe

type Board = Array Column
type Column = Array Space
data Space = Red | Black

instance showSpace :: Show Space where
  show Red = " Red "
  show Black = "Black"

col :: Column
col = [Red, Red, Black]

board :: Board
board = [[Red, Red, Red, Red],
         [Black, Black, Black, Black],
         [Red, Black, Black, Red]]


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show Red)
  log (show Black)
  traverse_ (log <<< foldMap ((++ " ") <<< show)) board


  log ("Thanks for playing!")
