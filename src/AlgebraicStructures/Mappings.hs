module AlgebraicStructures.Mappings (
  Mapping(..),
  Morphism(..)
) where

import AlgebraicStructures.Base
import Data.List.Extra

data Mapping a b =
  Injective  (a -> b) [a] [b] |
  Surjective (a -> b) [a] [b] |
  Bijective  (a -> b) [a] [b]

instance (Eq a, Eq b) => Ver (Mapping a b) where
  ver (Injective f a b)  = not (anySame c) && all (`elem` b) c
    where c = map f a
  ver (Surjective f a b) = all (`elem` c) b && all (`elem` b) c
    where c = map f a
  ver (Bijective f a b)  = ver (Injective f a b) && ver (Surjective f a b)

data Morphism a b =
    Homomorphism (a -> a -> a) (b -> b -> b) [a] [b] (a -> b) |
    Isomorphism  (a -> a -> a) (b -> b -> b) [a] [b] (a -> b)

instance (Eq a, Eq b) => Ver (Morphism a b) where
    ver (Homomorphism (+) (*) a b f) = and [f (x + y) == (f x) * (f y) && f (x + y) `elem` b | x <- a, y <- a]
    ver (Isomorphism  (+) (*) a b f) = ver (Homomorphism (+) (*) a b f) && ver (Bijective f a b)