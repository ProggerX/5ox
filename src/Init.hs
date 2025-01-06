module Init where

import Data.Array (listArray)
import GameLogic (Board, Cell (..), GameState (..), Status (..))
import System.Random (StdGen)

createBoard :: Board
createBoard = listArray ((1, 1), (5, 5)) (replicate 25 Empty)

createState :: StdGen -> Int -> GameState
createState randomGen difficulty =
  GameState
    { board = createBoard
    , cursor = (1, 1)
    , status = PlayerTurn
    , gameOver = False
    , botAttack = 4
    , botDefend = 5
    , randomGen
    , difficulty
    }
