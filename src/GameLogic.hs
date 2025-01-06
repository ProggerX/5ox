{-# LANGUAGE LambdaCase #-}

module GameLogic where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, put)
import Data.Array (Array, (//))
import Events (Direction (..), Event (..), parseEvent)

type Point = (Int, Int)
type Board = Array Point Cell

data Cell = X | O | Empty deriving (Show, Eq)
data Status = Win | Loss | Draw | PlayerTurn | AITurn deriving (Show, Eq)

data GameState = GameState
  { board :: Board
  , cursor :: Point
  , status :: Status
  , gameOver :: Bool
  }
  deriving (Show)

playerTurn :: (MonadState GameState m, MonadIO m) => m ()
playerTurn = do
  event <- parseEvent <$> liftIO getChar
  case event of
    None -> playerTurn
    Turn -> do
      st@GameState{cursor, board} <- get
      put st{board = board // [(cursor, X)]}
    MoveCursor dir -> do
      st@GameState{cursor} <- get
      put st{cursor = cursor' cursor dir}
      playerTurn
 where
  cursor' cur = \case
    DUp -> if fst cur > 1 then (fst cur - 1, snd cur) else cur
    DDown -> if fst cur < 5 then (fst cur + 1, snd cur) else cur
    DLeft -> if snd cur > 1 then (fst cur, snd cur - 1) else cur
    DRight -> if snd cur < 5 then (fst cur, snd cur + 1) else cur
