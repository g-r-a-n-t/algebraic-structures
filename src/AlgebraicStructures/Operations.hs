module AlgebraicStructures.Operations (
  identity,
  generate,
  generateBounded
) where

import Data.List
import GHC.Integer

identity :: Eq a => (a -> a -> a) -> [a] -> Maybe a
identity (+) d = find (\e -> all (\a -> e + a == a && a + e == a) d) d

generate :: Eq a => a -> (a -> a -> a) -> [a]
generate = generateBounded (-1)

generateBounded :: Eq a => Integer -> a -> (a -> a -> a) -> [a]
generateBounded = generateBounded' []

generateBounded' :: Eq a => [a] -> Integer -> a -> (a -> a -> a) -> [a]
generateBounded' _  0 _ _ = []
generateBounded' [] _ a _ = [a]
generateBounded' d n a (+)
  | a == a'   = d
  | otherwise = generateBounded' (a' : d) (minusInteger n 1) a (+)
  where a' = a + last d

