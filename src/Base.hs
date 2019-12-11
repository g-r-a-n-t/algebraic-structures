module Base (
  Ver(ver),
  Add(add),
  Mul(mul)
) where

class Ver a where
  ver :: a -> Bool

class Add a where
  add :: a -> a -> a

class Mul a where
  mul :: a -> a -> a

