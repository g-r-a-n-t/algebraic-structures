## Algebraic Structures

This library provides tools for working with finite algebraic structures. It is not built for any practical reasons and serves mostly for my (g-r-a-n-t's) personal education.

### Overview

This project has three main modules:

- Properties
- Structures
- Theorems

The properties module consists of basic properties that can be attributed to operations over finite domains. For example: *associativity*, *commutativity*, and *invertiblity*.

The structures module provides structures that contain certain properties. For example: *group*, *field*, and *ring*.

The theorem module provides theorems that make claims about structures and their properties. For example: *Every subgroup of a cyclic group is cyclic.*

Properties and structures are all verifiable, meaning you can prove them using brute force.

### Usage

**Build and test**

```sh
stack build
stack test
```

The structure, property, and theorem types provided here are instances of a typeclass called *Ver*. The Ver typeclass contains a function `ver` (`a -> Bool`) which verifies the integrity of its underlying value.

Take for example the associative property. If we have some function and a finite domain, we can verify that the associative property holds over all elements.

```haskell
ver (Associativity (+) d) = and [(a + b) + c == a + (b + c) | a <- d, b <- d, c <- d]
```

We can take this property along with others and construct structures that are also verifiable.

```haskell
ver (Group (+) d) = all ver [Closure (+) d, Associativity (+) d, Identity (+) d, Invertibility (+) d]
```

In practice this can be used to verify something like the group integers modulo 7 over addition.

```haskell
n7 = [0..6] :: [Mod Integer 7]
ver $ Group (+) n7
-- True
```

Furthermore, we can test theorems in some cases, but not absolutely prove them.

```haskell
-- Every subgroup of a cyclic group is cyclic.
ver (Theorem_4_10 g) = all (\(Group (+) d) -> ver $ Cyclical (+) d) (subgroups g)
```

Theorem names refer to the following [text](http://abstract.ups.edu/download/aata-20180801.pdf)

### Supported Types

```haskell
data Mapping a b =
  Injective  (a -> b) [a] [b] |
  Surjective (a -> b) [a] [b] |
  Bijective  (a -> b) [a] [b]

data Property a =
  Closure           (a -> a -> a) [a] |
  Associativity     (a -> a -> a) [a] |
  Commutativity     (a -> a -> a) [a] |
  Identity          (a -> a -> a) [a] |
  Invertibility     (a -> a -> a) [a] |
  Idempotency       (a -> a -> a) [a] |
  Cyclical          (a -> a -> a) [a] |
  Distributivity    (a -> a -> a) (a -> a -> a) [a] |
  AbsorbingZero     (a -> a -> a) (a -> a -> a) [a] |
  JacobiIdentity    (a -> a -> a) (a -> a -> a) [a] |
  MultInvertibility (a -> a -> a) (a -> a -> a) [a]

data GroupLike a =
  Magma        (a -> a -> a) [a] |
  Semigroup    (a -> a -> a) [a] |
  Monoid       (a -> a -> a) [a] |
  Group        (a -> a -> a) [a] |
  AbelianGroup (a -> a -> a) [a] |
  Semilattice  (a -> a -> a) [a]

data RingLike a =
  Semiring (a -> a -> a) (a -> a -> a) [a] |
  Nearring (a -> a -> a) (a -> a -> a) [a] |
  Ring     (a -> a -> a) (a -> a -> a) [a] |
  LieRing  (a -> a -> a) (a -> a -> a) [a] |
  BoolRing (a -> a -> a) (a -> a -> a) [a] |
  Field    (a -> a -> a) (a -> a -> a) [a]

data Morphism a b =
    Homomorphism (a -> a -> a) (b -> b -> b) [a] [b] (a -> b) |
    Isomorphism  (a -> a -> a) (b -> b -> b) [a] [b] (a -> b)

data Theorem a =
  Theorem_4_10 (GroupLike a)
```
