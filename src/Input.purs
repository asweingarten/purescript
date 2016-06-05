module Input where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.List
import Data.List.WordsLines
import Data.Int

import Types

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

