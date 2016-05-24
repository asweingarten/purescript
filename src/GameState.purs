module GameState where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log, print)
import Control.Monad.State.Trans
import Control.Monad.Eff.Class

import Data.Tuple
import Data.Maybe
import Data.List
import Data.List.WordsLines
import Data.Int
import Data.Foldable

import Types
import ReadLine

gameState :: forall e. Game e String
gameState = do
    loop
    move <- liftEff $ readLine
    liftEff $ log move
    return "Congrats someone won"
  where
    loop = do
      input <- liftEff readLine
      --liftEff $ log input
      let move = parseMove input
      case move of
           Nothing -> loop
           Just (Tuple space column) -> modify (addPiece space column)
      (board :: Board) <- get
      liftEff $ traverse_ print board
      -- if !winner
      loop

parseMove :: String -> Maybe (Tuple Space Int)
parseMove move = do
  let moveWords = words move
  space <- (head moveWords) >>= parseSpace
  position <- (last moveWords) >>= parsePosition
  return $ Tuple space position

parseSpace :: String -> Maybe Space
parseSpace s = case s of
                 "Red" -> Just Red
                 "Black" -> Just Black
                 _ -> Nothing

parsePosition :: String -> Maybe Int
parsePosition s = fromString s

addPiece :: Space -> Int -> Board -> Board
addPiece _ _ Nil = Nil
addPiece move 0 board =
  maybe (Nil) (pure <<< flip snoc move) (head board) ++ drop 1 board
addPiece move col board =
  maybe (Nil) (pure) (head board) ++ addPiece move (col-1) (drop 1 board)
{-- addPiece move col board = --}
{--   head board ++ addPiece move (col-1) (drop 1 board) --}

myInitialState :: Board
myInitialState = replicate 8 Nil

runMyShit :: forall e. UI e String
runMyShit = do
  result <- runStateT gameState myInitialState
  return ""

