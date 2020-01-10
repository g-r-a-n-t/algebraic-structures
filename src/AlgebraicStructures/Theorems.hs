module AlgebraicStructures.Theorems (
  Theorem(..)
) where

import AlgebraicStructures.Base
import AlgebraicStructures.Structures
import AlgebraicStructures.Properties

data Theorem a =
  Theorem_4_10 (GroupLike a)

instance Eq a => Ver (Theorem a) where
  ver (Theorem_4_10 g) = all (\(Group (+) d) -> ver $ Cyclical (+) d) (subgroups g)
