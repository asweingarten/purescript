module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, print)

import Data.Array
import Data.Foldable
import Data.Maybe

import Node.ReadLine (READLINE)
import Node.SimpleRepl (setPrompt, readLine, runRepl, putStrLn)

import Types (UI, Game, Board, Column, Space (..))
import GameState

col :: Column
col = [Red, Red, Black]

board :: Board
board = [[Red, Red, Red, Red],
         [Black, Black, Black, Black],
         [Red, Black, Black, Red]]


main :: forall e. UI e String
main = do
 runMyShit
