module Properties (
  Property
) where

import Base

data Property a = Closed [a] | Associative a | Commutative a | Identity a | Invertible a

instance (Eq a, Add a) => Ver (Property a) where
  ver (Closed d) = all (`elem` d) [a `add` b | a <- d, b <- d]

