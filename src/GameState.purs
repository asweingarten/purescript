module GameState where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log, print)
import Control.Monad.State.Trans
import Control.Monad.State.Class

import Node.ReadLine (READLINE)
import Node.SimpleRepl (setPrompt, readLine, runRepl, putStrLn)

import Types

type UI e = Eff (console :: CONSOLE, readline :: READLINE | e)
type Game e = StateT Board (UI e)

gameState :: forall e. Game e String
gameState = do
  liftEff $ runRepl do
    setPrompt "> "
    putStrLn "Connect 4"
    putStrLn ":q to quit"
    loop
  (z :: Board) <- get
  modify (addPiece Red 1)
  return "Dawgs"
 where
  loop = do
    res <- readLine
    case res of
         ":q" -> pure unit
         _ -> do
           putStrLn res
           loop

addPiece :: Space -> Int -> Board -> Board
addPiece move col board = board

myInitialState :: Board
myInitialState = [[],[],[],[],[],[],[],[]]

runMyShit :: forall e. UI e String
runMyShit = do
  result <- runStateT gameState myInitialState
  return ""

