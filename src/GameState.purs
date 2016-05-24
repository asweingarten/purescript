module GameState where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Trans
import Control.Monad.Eff.Class

import Data.Tuple
import Data.Maybe
import Data.List
import Data.List.WordsLines
import Data.Int

import Types
import ReadLine

gameState :: forall e. Game e String
gameState = do
  loop
  move <- liftEff $ readLine
  liftEff $ log move
  --(z :: Board) <- get
  --modify (addPiece Red 1)
  return "Congrats someone won"
 where
   loop = do
     move <- liftEff readLine
     liftEff $ log move
     let x = parseMove move
     -- parse move
     -- modify (addPiece move.Color move.col)
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
addPiece move col board = board

myInitialState :: Board
myInitialState = [[],[],[],[],[],[],[],[]]

runMyShit :: forall e. UI e String
runMyShit = do
  result <- runStateT gameState myInitialState
  return ""

