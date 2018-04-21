module Advent.Day5
where

import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , (!)
                                                )
import           Data.Maybe                     ( isNothing )

escapeMaze :: [Int] -> Int
escapeMaze maze = escapeMaze' 0 0 (Map.fromList (zip ([0 ..] :: [Int]) maze))

escapeMaze' :: Int -> Int -> Map Int Int -> Int
escapeMaze' jump jumps maze = if isNothing (maze !? jump)
  then jumps
  else escapeMaze' (jump + maze ! jump)
                   (jumps + 1)
                   (Map.alter ((+ 1) <$>) jump maze)
