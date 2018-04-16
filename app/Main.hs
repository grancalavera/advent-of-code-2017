module Main where
import System.IO (readFile)
import Data.Char (digitToInt)
import Advent.Day1

main :: IO ()
main = putStrLn "Advent of Code"

day1 :: IO ()
day1 = do
  contents <- readFile "day1input.txt"
  putStrLn $ show $ captcha $ map digitToInt $ concat $ lines contents
