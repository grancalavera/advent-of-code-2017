module Advent.Day1
where

captcha :: Int -> [Int] -> Int
captcha _      [] = 0
captcha offset xs = captcha' (zip [0 ..] xs) (cycle xs)
 where
  captcha' [] _ = 0
  captcha' ((i, y) : iys) xs | y == xs !! (i + offset) = y + captcha' iys xs
                             | otherwise               = captcha' iys xs
