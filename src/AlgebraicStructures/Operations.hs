module AlgebraicStructures.Operations (
  identity,
  generate,
  generateBounded,
  substructures
) where

import AlgebraicStructures.Base
import Data.List
import GHC.Integer

identity :: Eq a => (a -> a -> a) -> [a] -> Maybe a
identity (+) d = find (\e -> all (\a -> e + a == a && a + e == a) d) d

generate :: Eq a => (a -> a -> a) -> a -> [a]
generate = generateBounded (-1)

generateBounded :: Eq a => Integer -> (a -> a -> a) -> a -> [a]
generateBounded = generateBounded' []

generateBounded' :: Eq a => [a] -> Integer -> (a -> a -> a) -> a -> [a]
generateBounded' _  0 _  _ = []
generateBounded' [] _ _  a = [a]
generateBounded' d n (+) a
  | a == a'   = d
  | otherwise = generateBounded' (a' : d) (minusInteger n 1) (+) a
  where a' = a + last d

substructures :: (Eq a, Ver b) => ([a] -> b) -> [a] -> [b]
substructures s' d = filter ver (map s' (concat $ map (\n -> choose d n) [0..(length d) - 1]))

-- Copied from https://stackoverflow.com/questions/14267196/fast-obtention-of-all-the-subsets-of-size-n-in-haskell
choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k