{-# LANGUAGE RankNTypes #-}

module Conway
  ( step
  , conwaysRule
  , render
  , mkGrid
  ) where

import Data.List (intercalate)
import Store     (Store (..), experiment, extend, extract)
import Utils     (between, chunksOf, memoize)

type Coord = (Int, Int)

type Grid = Store Coord Bool

type Rule = Grid -> Bool

conwaysRule :: Int -> Int -> Rule
conwaysRule w h g = neigbourCount == 3 || (isAlive && neigbourCount == 2)
  where
    isAlive = extract g
    livingNeighbours = experiment (neighbours w h) g
    neigbourCount = length $ filter id livingNeighbours

neighbours :: Int -> Int -> Coord -> [Coord]
neighbours w h (a, b) =
  map
    (wrap . add (a, b))
    [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]
  where
    add (a, b) (x, y) = (x + a, y + b)
    wrap (x, y) = (x `mod` w, y `mod` h)

step :: Rule -> Grid -> Grid
step rule = flip extend rule . memoize

render :: Int -> Int -> Grid -> String
render w h = intercalate "\n" . map draw . chunksOf w . experiment (const cells)
  where
    cells = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
    draw =
      map
        (\x ->
           if x
             then 'X'
             else ' ')

mkGrid :: [Coord] -> Grid
mkGrid xs = Store (`elem` xs) (0, 0)
