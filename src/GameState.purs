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
  let verticalWinner   = findVerticalWinner board
      horizontalWinner = findHorizontalWinner board
      --diagonalWinner   = findDiagonalWinner board
   in verticalWinner <|> horizontalWinner -- <|> diagonalWinner

hasFourInRow :: forall a. Eq a => List a -> Maybe a
hasFourInRow moves =
  let streaks = groupBy (==) moves
  in
    case find (\s -> length s == 4) streaks of
       Nothing -> Nothing
       Just streak -> head streak

findVerticalWinner :: forall a. Eq a => Board a -> Maybe a
findVerticalWinner board =
  head $ mapMaybe (hasFourInRow) board

findHorizontalWinner :: forall a. Eq a => Board a -> Maybe a
findHorizontalWinner board =
  head $ mapMaybe (hasFourInRow) (transpose board)

findDiagonalWinner :: Board Move -> Maybe Move
findDiagonalWinner board =
  let maybefiedBoard = map (\c -> map (\m -> Just m) c) board
      boardLength    = length board
      leftRightDiag  = padColumns boardLength maybefiedBoard
      rightLeftDiag  = padColumns boardLength (reverse maybefiedBoard)
   in
      join (findHorizontalWinner leftRightDiag) <|> join (findHorizontalWinner rightLeftDiag)

padColumns :: Int -> Board (Maybe Move) -> Board (Maybe Move)
padColumns 0 _ = Nil
padColumns padHeight board =
  let padding = replicate padHeight Nothing
      h       = head board
      paddedHead = maybe Nil (\col -> col <> padding) h
      t       = maybe Nil id (tail board)
      paddedTail = (padColumns (padHeight - 1) t)
   in
      paddedHead : paddedTail

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

