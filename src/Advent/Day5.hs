{-# LANGUAGE BangPatterns #-}
module Advent.Day5
where

import           Data.Maybe                     ( isNothing )
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector
                                                , (!)
                                                , (!?)
                                                )

escapeMazeSpecial :: [Int] -> Int
escapeMazeSpecial maze = escapeMaze'' 0 0 (V.fromList maze)

escapeMaze :: [Int] -> Int
escapeMaze maze = escapeMaze' 0 0 (V.fromList maze)

escapeMaze' :: Int -> Int -> Vector Int -> Int
escapeMaze' !jump !jumps !maze = if isNothing (maze !? jump)
  then jumps
  else escapeMaze' (jump + offset)
                   (jumps + 1)
                   (V.update maze (V.fromList [(jump, newOffset offset)]))
 where
  offset = maze ! jump
  newOffset !o = o + 1

escapeMaze'' :: Int -> Int -> Vector Int -> Int
escapeMaze'' jump jumps maze = if isNothing (maze !? jump)
  then jumps
  else escapeMaze'' (jump' jump offset) (jumps' jumps) maze'
 where
  jump' !j !o = j + o
  jumps' !js = js + 1
  offset = maze ! jump
  maze'  = V.update maze (V.fromList [(jump, offset' offset)])
  offset' !o = if o > 2 then o - 1 else o + 1

{-
(0)  3   0  1   -3  |  0
(1)  3   0  1   -3  |  1
 2  (3)  0  1   -3  |  2
 2   2   0  1  (-3) |  3
 2  (2)  0  1   -2  |  4
 2   3   0 (1)  -2  |  5
 2   1   0  2  (-2) |  6
 2   1  (0) 2   -1  |  7
 2   1  (1) 2   -1  |  8
 2   1   2 (2)  -1  |  9
 2   1   3  2   -1  | 10
-}
