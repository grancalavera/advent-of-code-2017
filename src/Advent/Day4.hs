module Advent.Day4
where
import           Data.List                      ( sort )
import qualified Data.Set                      as Set
import qualified Data.List                     as List

validatePassPhrase :: [String] -> Bool
validatePassPhrase [] = False
validatePassPhrase xs = length xs == Set.size (Set.fromList xs)

validateNoAnagrams :: [String] -> Bool
validateNoAnagrams xs = validateNoAnagrams'
  [ (x, y) | x <- xs, y <- xs, x /= y ]
 where
  validateNoAnagrams' [] = True
  validateNoAnagrams' ((x, y) : xs') | x `isAnagramOf` y = False
                                     | otherwise = validateNoAnagrams' xs'

isAnagramOf :: String -> String -> Bool
isAnagramOf x y = List.sort x == List.sort y
