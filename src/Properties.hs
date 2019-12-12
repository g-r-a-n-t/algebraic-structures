module Properties (
  Property(..),
  Ver(ver)
) where

import Base

data Property a = AddClosed [a] | AddAssociative [a] | AddCommutative [a] | AddIdentity [a] | AddInvertible [a]

instance (Eq a, Add a) => Ver (Property a) where
  ver (AddClosed d) = all (`elem` d) [a `add` b | a <- d, b <- d]
  ver (AddAssociative d) = all (\(l, r) -> l == r) [((a `add` b) `add` c, a `add` (b `add` c)) | a <- d, b <- d, c <- d]
  ver (AddCommutative d) = all (\(l, r) -> l == r) [(a `add` b, b `add` a) | a <- d, b <- d]
