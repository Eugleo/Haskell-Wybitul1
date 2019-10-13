module Conway
  ( step
  , conwaysRule
  , render
  , mkGrid
  ) where

import Data.List (intercalate)
import Store     (Store (..), experiment, extend, extract)

type Coord = (Int, Int)

type Grid = Store Coord Bool

type Rule s = Store s Bool -> Bool

conwaysRule :: Int -> Int -> Rule Coord
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

between :: Ord a => a -> (a, a) -> Bool
between x (y, z) = y <= x && x <= z

step :: Rule s -> Store s Bool -> Store s Bool
step = flip extend

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

mkGrid :: [Coord] -> Grid
mkGrid xs = Store (`elem` xs) (0, 0)
