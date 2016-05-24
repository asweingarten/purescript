module GameState where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, print)
import Control.Monad.State.Trans
import Control.Monad.State.Class

import Node.ReadLine (READLINE)

import Types

type UI e = Eff (console :: CONSOLE, readline :: READLINE | e)
type Game e = StateT Board (UI e)

gameState :: forall e. Game e String
gameState = do
  liftEff (log "Yo")
  (z :: Board) <- get
  modify (addPiece Red 1)
  return "Dawgs"


addPiece :: Space -> Int -> Board -> Board
addPiece move col board =
  board

myInitialState :: Board
myInitialState = [[],[],[],[],[],[],[],[]]

runMyShit :: forall e. UI e String
runMyShit = do
  result <- runStateT gameState myInitialState
  return ""

