module GameState where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log, print)
import Control.Monad.State.Trans
import Control.Monad.Eff.Class

import Control.Bind
import Control.Alt

import Data.Tuple
import Data.Maybe
import Data.List
import Data.Foldable

import Types
import Input (parseInput)
import Output (printBoard)
import ReadLine (readLine)
import WinDetection (findWinner)

gameState :: forall e. Game e String
gameState = do
    winner <- loop
    return ("Congrats " ++ winner ++ " won")
  where
    loop :: forall e. Game e String
    loop = do
      input <- liftEff readLine
      case parseInput input of
        Nothing -> void loop
        Just (Tuple space column) -> modify (addPiece space column)
      (board :: Board Move) <- get
      liftEff $ printBoard board
      case findWinner board of
        Nothing -> loop
        Just winner -> return (show winner)

addPiece :: Move -> Int -> Board Move -> Board Move
addPiece _ _ Nil = Nil
addPiece move 0 board =
  maybe (Nil) (pure <<< flip snoc move) (head board) ++ drop 1 board
addPiece move col board =
  maybe (Nil) (pure) (head board) ++ addPiece move (col-1) (drop 1 board)

myInitialState :: Board Move
myInitialState = replicate 8 Nil

runMyShit :: forall e. UI e String
runMyShit = do
  result <- fst <$> runStateT gameState myInitialState
  return result

