module Main where

import App (run)
import System.IO (
  BufferMode (NoBuffering),
  hSetBinaryMode,
  hSetBuffering,
  hSetEcho,
  stdin,
  stdout,
 )

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  run
