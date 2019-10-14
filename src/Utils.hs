module Utils where

import Data.MemoCombinators (Memo (..), integral, pair)
import Store                (Store (..), experiment, extend, extract)

type Coord = (Int, Int)

memoize :: Store Coord a -> Store Coord a
memoize (Store f s) = Store (pair integral integral f) s

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

between :: Ord a => a -> (a, a) -> Bool
between x (y, z) = y <= x && x <= z

wrapAround :: (Int, Int) -> (Int, Int) -> (Int, Int)
wrapAround (w, h) (x, y) = (x `mod` w, y `mod` h)
