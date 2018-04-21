module Advent.Day5
where

escapeMaze :: [Int] -> Int
escapeMaze maze = escapeMaze' 0 0 (zip ([0 ..] :: [Int]) maze)

escapeMaze' :: Int -> Int -> [(Int, Int)] -> Int
escapeMaze' _    _     []   = 0
escapeMaze' jump jumps maze = if jump < 0 || jump >= length maze
  then jumps
  else
    let jump' = jump + snd (maze !! jump)
        maze' = map (\(i, x) -> if i == jump then (i, x + 1) else (i, x)) maze
    in  escapeMaze' jump' (jumps + 1) maze'
