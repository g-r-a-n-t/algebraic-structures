module AlgebraicStructures.Structures (
  GroupLike(..)
) where

import AlgebraicStructures.Properties
import AlgebraicStructures.Base

data GroupLike a =
  Magma        (a -> a -> a) [a] | -- S and a single binary operation over S.
  Semigroup    (a -> a -> a) [a] | -- an associative magma.
  Monoid       (a -> a -> a) [a] | -- a semigroup with Identity element.
  Group        (a -> a -> a) [a] | -- a monoid with an inverse for each element.
  AbelianGroup (a -> a -> a) [a] | -- a group whose binary operation is commutative.
  Semilattice  (a -> a -> a) [a]   -- a semigroup whose operation is idempotent and commutative.
  -- Quasigroup   (a -> a -> a) [a] | -- a magma obeying the latin square property. TODO: Add latin square property
  -- Loop         (a -> a -> a) [a]   -- A quasigroup with identity.

data RingLike a =
  Semiring (a -> a -> a) (a -> a -> a) [a] | -- Addition and multiplication over the domain form monoids, the operations
                                             -- addition is also commutative and both operations together are distributive.
  Nearring (a -> a -> a) (a -> a -> a) [a]   -- Addition and multiplication over the domain form an abelian group and
                                             -- monoid respectively, the operations together are distributive and the
                                             -- additive identity absorbs all elements by multiplication (0x=0 for all x in d).

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
  ver (Nearring (+) (*) d) = all ver [AbelianGroup (+) d, Monoid (*) d] &&
                             all ver [Distributivity (+) (*) d, AbsorbingZero (+) (*) d]