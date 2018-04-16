module Advent.Day1
where

captcha :: [Int] -> Int
captcha []       = 0
captcha (x : xs) = captcha' $ (x:xs) ++ [x]
 where
  captcha' [] = 0
  captcha' (x:xs) = captcha'' x xs
  captcha'' last [] = 0
  captcha'' last (next : rest) =
    if last == next then last + captcha'' next rest else captcha' (next : rest)
