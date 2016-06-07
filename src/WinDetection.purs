module WinDetection where

import Prelude

import Control.Bind
import Control.Alt
import Data.List
import Data.Maybe
import Data.Foldable

import Types (Board, Move, Column)

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

