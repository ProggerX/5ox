{-# LANGUAGE OverloadedRecordDot #-}

module App where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, evalStateT, get)
import GameLogic (GameState (..), Status (..), aiTurnStep, checkGameOver, playerTurnStep)
import Init (createState)
import Render (render)
import System.Random (getStdGen)

newtype App m a = App {runApp :: StateT GameState m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState GameState
    , MonadIO
    )

gameloop :: (MonadState GameState m, MonadIO m) => m ()
gameloop = do
  st <- get
  render
  if st.status == PlayerTurn then playerTurnStep else aiTurnStep
  render
  checkGameOver
  st' <- get
  unless st'.gameOver gameloop
  liftIO $
    when st'.gameOver $
      putStrLn
        ( case st'.status of
            Win -> "You win!"
            Loss -> "You've lost!"
            Draw -> "Draw! Not bad!"
            _ -> ""
        )

run :: Int -> IO ()
run d = do
  gen <- getStdGen
  (`evalStateT` createState gen d) $ runApp gameloop
