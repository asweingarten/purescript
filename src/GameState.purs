module GameState where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log, print)
import Control.Monad.State.Trans
import Control.Monad.Eff.Class

import Control.Alt

import Control.Bind

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
      liftEff $ traverse_ (log <<< printCol) board
      case findWinner board of
        Nothing -> loop
        Just winner -> return (show winner)

findWinner :: Board Move -> Maybe Move
findWinner board =
  let verticalWinner = head (mapMaybe hasFourInRow board)
      horizontalWinner = head (mapMaybe hasFourInRow (transpose board))
  in verticalWinner <|> horizontalWinner

{--findDiagonalWinner :: Board -> Maybe Move
findDiagonalWinner board =
  let indexedCols = zip (0 .. 8) board
      dexedniCols = zip (8 .. 0) board
      leftToRight = map dropMoves indexedCols
      rightToLeft = map dropMoves dexedniCols
  in
    map dropMoves indexedCols
    Nothing

dropMoves :: Tuple Int Column -> Column
dropMoves amount col =
  drop amount col
  --}
hasFourInRow :: List Move -> Maybe Move
hasFourInRow moves =
  let streaks = groupBy (==) moves
  in
    case find (\s -> length s == 4) streaks of
       Nothing -> Nothing
       Just streak -> head streak

printCol :: Column Move -> String
printCol col =
  foldMap show col

parseInput :: String -> Maybe (Tuple Move Int)
parseInput move = do
  let moveWords = words move
  space <- (head moveWords) >>= parseMove
  position <- (last moveWords) >>= parsePosition
  return $ Tuple space position

parseMove :: String -> Maybe Move
parseMove s = case s of
                 "Red" -> Just Red
                 "Black" -> Just Black
                 _ -> Nothing

parsePosition :: String -> Maybe Int
parsePosition s = fromString s

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

