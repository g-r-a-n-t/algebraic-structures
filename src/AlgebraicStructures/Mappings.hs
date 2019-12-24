module AlgebraicStructures.Mappings (
  Mapping(..)
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