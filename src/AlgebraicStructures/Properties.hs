module AlgebraicStructures.Properties (
  Property(..)
) where

import AlgebraicStructures.Base
import Data.Maybe

data Property a =
  Closure         (a -> a -> a) [a] |
  Associativity   (a -> a -> a) [a] |
  Commutativity   (a -> a -> a) [a] |
  Identity        (a -> a -> a) [a] |
  Invertibility   (a -> a -> a) [a] |
  Idempotency     (a -> a -> a) [a] |
  Distributivity  (a -> a -> a) (a -> a -> a) [a] |
  AbsorbingZero   (a -> a -> a) (a -> a -> a) [a]

instance Eq a => Ver (Property a) where
  ver (Closure        (+) d) = and [elem (a + b) d | a <- d, b <- d]
  ver (Associativity  (+) d) = and [(a + b) + c == a + (b + c) | a <- d, b <- d, c <- d]
  ver (Commutativity  (+) d) = and [a + b == b + a | a <- d, b <- d]
  ver (Identity       (+) d) = isJust (identity (+) d)
  ver (Invertibility  (+) d) = fmap (\e -> all (\a -> any (\b -> a + b == e) d) d) (identity (+) d) == Just True
  ver (Idempotency    (+) d) = all (\x -> x + x == x) d
  ver (Distributivity (+) (*) d) = and [a * (b + c) == (a * b) + (a * c) | a <- d, b <- d, c <- d]
  ver (AbsorbingZero  (+) (*) d) = fmap (\e -> all (\a -> e * a == e) d) (identity (+) d) == Just True

identity :: Eq a => (a -> a -> a) -> [a] -> Maybe a
identity (*) d
  | length es /= (1 :: Int) = Nothing
  | otherwise = Just (es!!0)
  where es = filter (\e -> all (\a -> e * a == a && a * e == a) d) d
