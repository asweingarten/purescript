module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, print)

import Data.Array
import Data.Foldable
import Data.Maybe

import Node.ReadLine (READLINE)
import Node.SimpleRepl (setPrompt, readLine, runRepl, putStrLn)

import Types (Board, Column, Space (..))

col :: Column
col = [Red, Red, Black]

board :: Board
board = [[Red, Red, Red, Red],
         [Black, Black, Black, Black],
         [Red, Black, Black, Red]]


main :: forall e. Eff (console :: CONSOLE, readline :: READLINE | e) Unit
main = runRepl do
  {- log (show Red)
  log (show Black)
  traverse_ (log <<< foldMap ((_ ++ " ") <<< show)) board -}
  setPrompt "> "
  putStrLn "Connect 4"
  putStrLn ":q to quit"
  loop
    where
      loop = do
        res <- readLine
        case res of
             ":q" -> pure unit
             _ -> do
               putStrLn res
               loop
