module App where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, evalStateT, get)
import GameLogic (GameState, playerTurn)
import Init (createState)

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
  liftIO $ print st
  playerTurn
  st' <- get
  liftIO $ print st'

run :: IO ()
run = do
  (`evalStateT` createState) $ runApp gameloop
