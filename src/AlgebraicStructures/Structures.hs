module AlgebraicStructures.Structures (
  GroupLike(..)
) where

import AlgebraicStructures.Properties
import AlgebraicStructures.Base

data GroupLike a =
  Magma        (a -> a -> a) [a] | -- S and a single binary operation over S.
  Semigroup    (a -> a -> a) [a] | -- an associative magma.
  Monoid       (a -> a -> a) [a] | -- a semigroup with Identity element.
  Group        (a -> a -> a) [a] | -- a monoid with a unary operation (inverse), giving rise to inverse elements.
  AbelianGroup (a -> a -> a) [a] | -- a group whose binary operation is commutative.
  Semilattice  (a -> a -> a) [a] | -- a semigroup whose operation is idempotent and commutative.
  Quasigroup   (a -> a -> a) [a] | -- a magma obeying the latin square property.
  Loop         (a -> a -> a) [a]   -- A quasigroup with identity.

instance Eq a => Ver (GroupLike a) where
  ver (Magma     f d) = all ver [Closure f d]
  ver (Semigroup f d) = all ver [Closure f d, Associativity f d]
  ver (Monoid    f d) = all ver [Closure f d, Associativity f d]
  ver (Group     f d) = all ver [Closure f d, Associativity f d, Commutativity f d]

