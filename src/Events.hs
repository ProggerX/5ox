{-# LANGUAGE LambdaCase #-}

module Events where

data Direction = DLeft | DRight | DUp | DDown deriving (Eq)

data Event = MoveCursor Direction | Turn | None deriving (Eq)

parseEvent :: Char -> Event
parseEvent = \case
  'd' -> MoveCursor DRight
  'a' -> MoveCursor DLeft
  's' -> MoveCursor DDown
  'w' -> MoveCursor DUp
  ' ' -> Turn
  _ -> None
