module AlgebraicStructures.Base (
  Ver(ver)
) where

class Ver a where
  ver :: a -> Bool

