{-# LANGUAGE BangPatterns #-}
module Advent.Day5
where

import           Data.Maybe                     ( isNothing )
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector
                                                , (!)
                                                , (!?)
                                                )

escapeMaze :: (Int -> Int) -> [Int] -> Int
escapeMaze updateOffset maze = escapeMaze' updateOffset 0 0 (V.fromList maze)

escapeMaze' :: (Int -> Int) -> Int -> Int -> Vector Int -> Int
escapeMaze' updateOffset !jump !jumps !maze = if isNothing (maze !? jump)
  then jumps
  else
    let !offset = maze ! jump
    in  escapeMaze' updateOffset
                    (jump + offset)
                    (jumps + 1)
                    (V.update maze (V.fromList [(jump, updateOffset offset)]))

simpleOffset :: Int -> Int
simpleOffset !o = o + 1

specialOffset :: Int -> Int
specialOffset !o | o > 2     = o - 1
                 | otherwise = o + 1
