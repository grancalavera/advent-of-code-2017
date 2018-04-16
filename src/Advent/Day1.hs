module Advent.Day1
where

captcha :: [Int] -> Int
captcha []       = 0
captcha (x : xs) = captcha' $ (x : xs) ++ [x]
 where
  captcha' []       = 0
  captcha' (x : xs) = captcha'' x xs
  captcha'' last [] = 0
  captcha'' last (next : rest) =
    if last == next then last + captcha'' next rest else captcha' (next : rest)

kaptcha :: [Int] -> Int
kaptcha [] = 0
kaptcha xs = kaptcha' (length xs `div` 2) (zip [0 ..] xs) (cycle xs)
 where
  kaptcha' _ [] _ = 0
  kaptcha' offset ((i, y) : iys) xs
    | y == xs !! (i + offset) = y + kaptcha' offset iys xs
    | otherwise               = kaptcha' offset iys xs
