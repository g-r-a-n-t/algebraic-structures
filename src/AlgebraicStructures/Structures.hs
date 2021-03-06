module AlgebraicStructures.Structures (
  GroupLike(..),
  RingLike(..),
  subgroups
) where

import Data.Maybe
import AlgebraicStructures.Properties
import AlgebraicStructures.Operations
import AlgebraicStructures.Base

data GroupLike a =
  Magma        (a -> a -> a) [a] |
  Semigroup    (a -> a -> a) [a] |
  Monoid       (a -> a -> a) [a] |
  Group        (a -> a -> a) [a] |
  AbelianGroup (a -> a -> a) [a] |
  Semilattice  (a -> a -> a) [a]
  -- Quasigroup   (a -> a -> a) [a] | TODO: Add latin square property
  -- Loop         (a -> a -> a) [a]

data RingLike a =
  Semiring (a -> a -> a) (a -> a -> a) [a] |
  Nearring (a -> a -> a) (a -> a -> a) [a] |
  Ring     (a -> a -> a) (a -> a -> a) [a] |
  LieRing  (a -> a -> a) (a -> a -> a) [a] |
  BoolRing (a -> a -> a) (a -> a -> a) [a] |
  Field    (a -> a -> a) (a -> a -> a) [a]

data ModuleLike a b =
  LeftModule (GroupLike a) (RingLike b) (b -> a -> a)

instance Eq a => Ver (GroupLike a) where
  ver (Magma        (+) d) = all ver [Closure (+) d]
  ver (Semigroup    (+) d) = all ver [Closure (+) d, Associativity (+) d]
  ver (Monoid       (+) d) = all ver [Closure (+) d, Associativity (+) d, Identity (+) d]
  ver (Group        (+) d) = all ver [Closure (+) d, Associativity (+) d, Identity (+) d, Invertibility (+) d]
  ver (AbelianGroup (+) d) = all ver [Closure (+) d, Associativity (+) d, Identity (+) d, Invertibility (+) d, Commutativity (+) d]
  ver (Semilattice  (+) d) = all ver [Closure (+) d, Associativity (+) d, Idempotency (+) d, Commutativity (+) d]

instance Eq a => Ver (RingLike a) where
  ver (Semiring (+) (*) d) = all ver [Monoid (+) d, Monoid (*) d] &&
                             all ver [Commutativity (+) d, Distributivity (+) (*) d]
  ver (Nearring (+) (*) d) = all ver [Group (+) d, Monoid (*) d] &&
                             all ver [Distributivity (+) (*) d, AbsorbingZero (+) (*) d]
  ver (Ring     (+) (*) d) = all ver [AbelianGroup (+) d, Monoid (*) d] &&
                             all ver [Distributivity (+) (*) d, AbsorbingZero (+) (*) d]
  ver (LieRing  (+) (*) d) = all ver [AbelianGroup (+) d, Magma (*) d] &&
                             all ver [Distributivity (+) (*) d, JacobiIdentity (+) (*) d]
  ver (BoolRing (+) (*) d) = all ver [AbelianGroup (+) d, Magma (*) d] &&
                             all ver [Distributivity (+) (*) d, Idempotency (*) d]
  ver (Field    (+) (*) d) = all ver [AbelianGroup (+) d, Magma (*) d] &&
                             all ver [Distributivity (+) (*) d, MultInvertibility (+) (*) d]

instance (Eq a, Eq b) => Ver (ModuleLike a b) where
  ver (LeftModule (Group (+) m) (Ring (++) (*) _r) (**)) =
    and [ r ** (x + y) == (r ** x) + (r ** y) | x <- m, y <- m, r <- _r ] &&
    and [(r ++ s) ** x == (r ** x) + (s ** x) | r <- _r, s <- _r, x <- m] &&
    and [(r * s) ** x == r ** (s ** x)        | r <- _r, s <- _r, x <- m] &&
    and [e ** x == x | x <- m]
    where e = fromJust (multIdentity (++) (*) _r)

subgroups (Group (+) d) = substructures (Group (+)) d