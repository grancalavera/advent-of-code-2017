module Main
where
import           System.IO                      ( readFile )
import           Data.Char                      ( digitToInt )
import           Data.List.Split                ( splitOn )
import           Advent.Day1
import           Advent.Day2
import           Advent.Day4
import           Advent.Day5

main :: IO ()
main = do
  header "Advent of Code"
  day1
  day2
  day4
  day5

day1 :: IO ()
day1 = do
  header "Day 1"
  contents <- readFile "day1input.txt"
  let contents' = concat $ lines contents

  header "Part 1"
  print (captcha 1 $ map digitToInt $ concat $ lines contents)

  header "Part 2"
  print
    (captcha (length contents' `div` 2) $ map digitToInt $ concat $ lines
      contents
    )

day2 :: IO ()
day2 = do
  header "Day 2"
  contents <- readFile "day2input.txt"
  let spreadsheet = map (map read) $ map (splitOn "\t") $ lines contents

  header "Part 1"
  print $ checksum spreadsheet

  header "Part 2"
  print $ divsum spreadsheet

day4 :: IO ()
day4 = do
  header "Day 4"
  contents <- readFile "day4input.txt"
  let passphrases = map (splitOn " ") $ lines contents

  header "Part 1"
  print $ length $ filter validatePassPhrase passphrases

  header "Part 2"
  print $ length $ filter validateNoAnagrams $ filter validatePassPhrase
                                                      passphrases

day5 :: IO ()
day5 = do
  header "Day 5"
  contents <- readFile "day5input.txt"
  let instructions = (map read $ lines contents) :: [Int]
  print $ escapeMaze instructions

header :: String -> IO ()
header = putStrLn . ("\n" ++)
