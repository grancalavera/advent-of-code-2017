module Advent.Day2
where

import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )

checksum :: [[Int]] -> Int
checksum xs = fromJust $ sum <$> mapM checksumRow xs

checksumRow :: [Int] -> Maybe Int
checksumRow xs = checksumRow' Nothing Nothing xs

checksumRow' :: Maybe Int -> Maybe Int -> [Int] -> Maybe Int
checksumRow' mn mx []       = (-) <$> mx <*> mn
checksumRow' mn mx (x : xs) = checksumRow' mn' mx' xs
 where
  px  = pure x
  mn' = if isNothing mn then px else min <$> mn <*> px
  mx' = if isNothing mx then px else max <$> mx <*> px
