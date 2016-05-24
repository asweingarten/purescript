module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, print)

import Node.ReadLine (READLINE)
import Node.SimpleRepl (setPrompt, readLine, runRepl, putStrLn)

import Types (UI, Game, Board, Column, Space (..))
import GameState

main :: forall e. UI e String
main = do
 runMyShit
