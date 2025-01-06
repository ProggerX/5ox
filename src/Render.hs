{-# LANGUAGE LambdaCase #-}

module Render where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get)
import Data.Array (assocs)
import Data.Foldable (foldl')
import GameLogic (Board, Cell (..), GameState (..), Point)

blueString :: String -> String
blueString str = mconcat ["\ESC[34m", str, "\ESC[0m"]

redString :: String -> String
redString str = mconcat ["\ESC[31m", str, "\ESC[0m"]

renderCell :: Bool -> Cell -> String
renderCell = \case
  False -> fst . show'
  True -> colorful . show'
 where
  show' = \case
    Empty -> ("-", Empty)
    X -> ("X", X)
    O -> ("O", O)
  colorful :: (String, Cell) -> String
  colorful (txt, cell) = case cell of
    Empty -> blueString "X"
    _ -> redString txt

renderBoard :: Point -> Board -> String
renderBoard cursor =
  foldl'
    ( \old (pt, typ) ->
        mconcat
          [ old
          , renderCell (pt == cursor) typ
          , if snd pt == 5 then "\n" else " "
          ]
    )
    ""
    . assocs

render :: (MonadIO m, MonadState GameState m) => m ()
render = do
  GameState{board, cursor} <- get
  liftIO $
    putStrLn $
      mconcat
        [ "\ESC[2J"
        , renderBoard cursor board
        ]
