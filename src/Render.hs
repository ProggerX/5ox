module Render where

import GameLogic (Cell, Point)

blueString :: String -> String
blueString str = mconcat ["\ESC[34m", str, "\ESC[0m"]

-- Given cursor poition and cell type produces a string with this cell
renderCell :: Point -> Cell -> String
renderCell = undefined
