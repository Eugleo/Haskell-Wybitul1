module Main where

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)
import Conway             (conwaysRule, mkGrid, render, step)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let start = mkGrid [(3, 3), (3, 4), (3, 5)]
  let (w, h) = (read $ head args, read $ args !! 1)
  forM_ (iterate (step (conwaysRule w h)) start) $ \grid -> do
    putStr "\ESC[2J"
    putStrLn (render w h grid)
    threadDelay 500000
