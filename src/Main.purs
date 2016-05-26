module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, print)

import Types (UI, Game, Board, Column, Move (..))
import GameState (runMyShit)

main :: forall e. UI e String
main = do
 runMyShit
