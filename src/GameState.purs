module GameState where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.State.Trans
import Control.Monad.Eff.Class

import Types
import ReadLine

gameState :: forall e. Game e String
gameState = do
  move <- liftEff $ readLine
  liftEff $ log move
  (z :: Board) <- get
  modify (addPiece Red 1)
  return "Dawgs"

addPiece :: Space -> Int -> Board -> Board
addPiece move col board = board

myInitialState :: Board
myInitialState = [[],[],[],[],[],[],[],[]]

runMyShit :: forall e. UI e String
runMyShit = do
  result <- runStateT gameState myInitialState
  return ""

