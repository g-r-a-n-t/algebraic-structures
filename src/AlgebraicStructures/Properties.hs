module AlgebraicStructures.Properties (
  Property(..)
) where

import AlgebraicStructures.Base

data Property a =
  Closure      (a -> a -> a) [a] |
  Associativity (a -> a -> a) [a] |
  Commutativity (a -> a -> a) [a] |
  Identity    (a -> a -> a) [a] |
  Invertibility  (a -> a -> a) [a]

instance Eq a => Ver (Property a) where
  ver (Closure      (*) d) = all (`elem` d) [a * b | a <- d, b <- d]
  ver (Associativity (*) d) = all (\(l, r) -> l == r) [((a * b) * c, a * (b * c)) | a <- d, b <- d, c <- d]
  ver (Commutativity (*) d) = all (\(l, r) -> l == r) [(a * b, b * a) | a <- d, b <- d]
