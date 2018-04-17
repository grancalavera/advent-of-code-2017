module Main
where
import           System.IO                      ( readFile )
import           Data.Char                      ( digitToInt )
import           Advent.Day1

main :: IO ()
main = putStrLn "Advent of Code"

day1 :: IO ()
day1 = do
  contents <- readFile "day1input.txt"
  let contents' = concat $ lines contents

  putStrLn "Part 1"
  print (captcha 1 $ map digitToInt $ concat $ lines contents)

  putStrLn "Part 2"
  print (captcha (length contents' `div` 2) $ map digitToInt $ concat $ lines contents)
