{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GameLogic where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, put)
import Data.Array (Array, indices, (!), (//))
import Data.List (sort)
import Events (Direction (..), Event (..), parseEvent)
import System.Random (StdGen, randomR)

type Point = (Int, Int)
type Board = Array Point Cell
data Cell = X | O | Empty deriving (Show, Eq)
data Status = Win | Loss | Draw | PlayerTurn | AiTurn deriving (Show, Eq)

data GameState = GameState
  { board :: Board
  , cursor :: Point
  , status :: Status
  , gameOver :: Bool
  , botAttack :: Int
  , botDefend :: Int
  , randomGen :: StdGen
  , difficulty :: Int
  }
  deriving (Show)

playerTurnStep :: (MonadState GameState m, MonadIO m) => m ()
playerTurnStep = do
  event <- parseEvent <$> liftIO getChar
  case event of
    None -> pure ()
    Turn -> do
      st@GameState{cursor, board} <- get
      when (board ! cursor == Empty) $ put st{board = board // [(cursor, X)], status = AiTurn}
    MoveCursor dir -> do
      st@GameState{cursor} <- get
      put st{cursor = cursor' cursor dir}
 where
  cursor' cur = \case
    DUp -> if fst cur > 1 then (fst cur - 1, snd cur) else cur
    DDown -> if fst cur < 5 then (fst cur + 1, snd cur) else cur
    DLeft -> if snd cur > 1 then (fst cur, snd cur - 1) else cur
    DRight -> if snd cur < 5 then (fst cur, snd cur + 1) else cur

aiTurnStep :: (MonadState GameState m) => m ()
aiTurnStep = do
  st@GameState{board, difficulty, botDefend, botAttack} <- get
  let profits = reverse $ filter (\x -> (board ! x) == Empty) $ map snd $ sort $ map (cellProfit board botDefend botAttack) (indices board)
  let difficulty' = min difficulty $ length profits
  put st{difficulty = difficulty'}
  let pts = take difficulty' profits
  let (i, randomGen) = randomR (0, difficulty' - 1) st.randomGen
  let pt = pts !! i
  put st{board = board // [(pt, O)], status = PlayerTurn, randomGen}

vertical, horizontal :: Int -> [Point]
vertical x = [(y, x) | y <- [1 .. 5]]
horizontal y = [(y, x) | x <- [1 .. 5]]

diagonal1, diagonal2 :: [Point]
diagonal1 = [(x, x) | x <- [1 .. 5]]
diagonal2 = [(x, 6 - x) | x <- [1 .. 5]]

cellProfit :: Board -> Int -> Int -> Point -> (Int, Point)
cellProfit brd def atk pt@(y, x) =
  ( if
      | y == x && y == 6 - x -> 10000
      | y == x ->
          let (vx, vo) = calculate brd $ vertical x
              (hx, ho) = calculate brd $ horizontal y
              (d1x, d1o) = calculate brd diagonal1
           in ( (if vx == 0 then vo * atk else if vo == 0 then vx * def else 0)
                  + (if hx == 0 then ho * atk else if ho == 0 then hx * def else 0)
                  + (if d1x == 0 then d1o * atk else if d1o == 0 then d1x * def else 0)
              )
      | y == 6 - x ->
          let (vx, vo) = calculate brd $ vertical x
              (hx, ho) = calculate brd $ horizontal y
              (d2x, d2o) = calculate brd diagonal2
           in ( (if vx == 0 then vo * atk else if vo == 0 then vx * def else 0)
                  + (if hx == 0 then ho * atk else if ho == 0 then hx * def else 0)
                  + (if d2x == 0 then d2o * atk else if d2o == 0 then d2x * def else 0)
              )
      | otherwise ->
          let (vx, vo) = calculate brd $ vertical x
              (hx, ho) = calculate brd $ horizontal y
           in ( (if vx == 0 then vo * atk else if vo == 0 then vx * def else 0)
                  + (if hx == 0 then ho * atk else if ho == 0 then hx * def else 0)
              )
  , pt
  )

(++?) :: Point -> Point -> Point
(++?) (a, b) (c, d) = (max a c, max b d)

checkLines :: Board -> Point -> (Int, Int)
checkLines brd (y, x) =
  calculate brd (vertical x)
    ++? calculate brd (horizontal y)
    ++? (if y == x then calculate brd diagonal1 else (0, 0))
    ++? (if y == 6 - x then calculate brd diagonal2 else (0, 0))

calculate :: Board -> [Point] -> (Int, Int)
calculate brd =
  foldl
    ( \(xs, os) pt ->
        ( case brd ! pt of
            X -> (xs + 1, os)
            O -> (xs, os + 1)
            _ -> (xs, os)
        )
    )
    (0, 0)

checkGameOver :: (MonadState GameState m) => m ()
checkGameOver = do
  st@GameState{board} <- get
  let st' = st{gameOver = True}
  if
    | any ((== 5) . fst . checkLines board) [(y, x) | x <- [1 .. 5], y <- [1 .. 5]] -> do
        put st'{status = Win}
    | any ((== 5) . snd . checkLines board) [(y, x) | x <- [1 .. 5], y <- [1 .. 5]] -> do
        put st'{status = Loss}
    | not $ any ((== Empty) . (board !)) [(y, x) | x <- [1 .. 5], y <- [1 .. 5]] -> do
        put st'{status = Draw}
    | otherwise -> pure ()
