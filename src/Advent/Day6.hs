module Advent.Day6
where

import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector )
import           Data.List                      ( foldl' )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

reallocate :: [Int] -> Int
reallocate = Set.size . reallocate' mempty

reallocate' :: Set [Int] -> [Int] -> Set [Int]
reallocate' seen memory | Set.member memory seen = seen
                        | otherwise              = undefined

cycle ::

largest :: [Int] -> (Int, Int)
largest = foldl' compare (-1, min) . zip [0 ..]
 where
  min = minBound :: Int
  compare old@(_, x) new@(_, x') | x < x'    = new
                                 | otherwise = old
