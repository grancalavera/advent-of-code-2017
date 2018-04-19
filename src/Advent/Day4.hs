module Advent.Day4
where
import           Data.List                      ( sort )
import qualified Data.Set                      as Set

validatePassPhrase :: [String] -> Bool
validatePassPhrase [] = False
validatePassPhrase xs = length xs == (Set.size $ Set.fromList xs)
