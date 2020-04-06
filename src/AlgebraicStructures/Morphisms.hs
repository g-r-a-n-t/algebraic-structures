module AlgebraicStructures.Morphisms (
    Morphism(..)
) where

import AlgebraicStructures.Base
import AlgebraicStructures.Mappings

data Morphism a b =
    Homomorphism (a -> a -> a) (b -> b -> b) [a] [b] (a -> b) |
    Isomorphism  (a -> a -> a) (b -> b -> b) [a] [b] (a -> b)

instance (Eq a, Eq b) => Ver (Morphism a b) where
    ver (Homomorphism (+) (*) a b f) = and [f (x + y) == (f x) * (f y) && f (x + y) `elem` b | x <- a, y <- a]
    ver (Isomorphism  (+) (*) a b f) = ver (Homomorphism (+) (*) a b f) && ver (Bijective f a b)
