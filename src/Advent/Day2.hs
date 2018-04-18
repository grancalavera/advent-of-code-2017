module Advent.Day2
where

import           Data.Maybe                     ( isNothing )

checksumRow :: [Int] -> Maybe Int
checksumRow _ = Just 0

checksumRow' :: Maybe Int -> Maybe Int -> [Int] -> Maybe Int
checksumRow' mn mx []       = (-) <$> mx <*> mn
checksumRow' mn mx (x : xs) = checksumRow' mn' mx' xs
 where
  mn' = choose min mn (pure x)
  mx' = choose max mx (pure x)
  choose f mx px = if isNothing mx then px else f <$> mn <*> px
