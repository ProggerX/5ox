module Init where

import Data.Array (listArray)
import GameLogic (Board, Cell (..), GameState (..), Status (..))

createBoard :: Board
createBoard = listArray ((1, 1), (5, 5)) (replicate 25 Empty)

createState :: GameState
createState =
  GameState
    { board = createBoard
    , cursor = (1, 1)
    , status = PlayerTurn
    , gameOver = False
    }
