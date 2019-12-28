module AlgebraicStructures.Properties (
  Property(..)
) where

import AlgebraicStructures.Base
import Data.Maybe
import Data.List
import GHC.Integer

data Property a =
  Closure           (a -> a -> a) [a] |
  Associativity     (a -> a -> a) [a] |
  Commutativity     (a -> a -> a) [a] |
  Identity          (a -> a -> a) [a] |
  Invertibility     (a -> a -> a) [a] |
  Idempotency       (a -> a -> a) [a] |
  Distributivity    (a -> a -> a) (a -> a -> a) [a] |
  AbsorbingZero     (a -> a -> a) (a -> a -> a) [a] |
  JacobiIdentity    (a -> a -> a) (a -> a -> a) [a] |
  MultInvertibility (a -> a -> a) (a -> a -> a) [a]

instance Eq a => Ver (Property a) where
  ver (Closure           (+) d) = and [elem (a + b) d | a <- d, b <- d]
  ver (Associativity     (+) d) = and [(a + b) + c == a + (b + c) | a <- d, b <- d, c <- d]
  ver (Commutativity     (+) d) = and [a + b == b + a | a <- d, b <- d]
  ver (Identity          (+) d) = isJust (identity (+) d)
  ver (Invertibility     (+) d) = fmap (\e -> all (\a -> any (\b -> a + b == e) d) d) (identity (+) d) == Just True
  ver (Idempotency       (+) d) = all (\x -> x + x == x) d
  ver (Distributivity    (+) (*) d) = and [a * (b + c) == (a * b) + (a * c) | a <- d, b <- d, c <- d]
  ver (AbsorbingZero     (+) (*) d) = fmap (\e -> all (\a -> e * a == e) d) (identity (+) d) == Just True
  ver (JacobiIdentity    (+) (*) d) = fmap (\e -> and [(a * (b * c)) + (b * (c * a)) + (c * (a * b)) == e | a <- d, b <- d, c <- d]) (identity (+) d) == Just True
  ver (MultInvertibility (+) (*) d) = fmap (\e -> ver (Invertibility (*) (filter (/=e) d))) (identity (+) d) == Just True

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

