module Advent.Day6
where

import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector
                                                , (!)
                                                )
import           Data.List                      ( foldl' )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

reallocate :: [Int] -> Int
reallocate [] = 0
reallocate xs = S.size $ reallocate' mempty $ V.fromList xs

reallocate' :: Set (Vector Int) -> Vector Int -> Set (Vector Int)
reallocate' seen memory
  | S.member memory seen
  = seen
  | otherwise
  = let (seen', memory') = step seen memory in reallocate' seen' memory'

step :: Set (Vector Int) -> Vector Int -> (Set (Vector Int), Vector Int)
step seen memory =
  let (bank, blocks) = largest memory
      bank'          = nextBank bank memory
      memory'        = redist bank' blocks (erase bank memory)
  in  (S.insert memory seen, memory')

erase :: Int -> Vector Int -> Vector Int
erase bank memory = V.update memory (V.fromList [(bank, 0)])

redist :: Int -> Int -> Vector Int -> Vector Int
redist bank blocks memory | blocks == 0 = memory
                          | otherwise   = redist bank' blocks' memory'
 where
  bank'   = nextBank bank memory
  blocks' = blocks - 1
  memory' = V.update memory $ V.fromList [(bank, memory ! bank + 1)]

largest :: Vector Int -> (Int, Int)
largest = V.ifoldl' compare (-1, minBound :: Int)
 where
  compare (i, x) i' x' | x < x'    = (i', x')
                       | otherwise = (i, x)

nextBank :: Int -> Vector Int -> Int
nextBank bank memory = (bank + 1) `mod` V.length memory
