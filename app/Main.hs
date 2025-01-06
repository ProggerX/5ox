module Main where

import App (run)
import System.Environment
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

  args :: [Int] <- fmap read <$> getArgs
  if length args == 1 then run $ head args else putStrLn "Expected one argument - difficulty (1-3 is recommended)"
