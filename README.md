## Algebraic Structures

This library provides types for various algebraic structures and properties and allows you to easily verify them (if reasonably small).

The structure and property types provided here are both instances of a typeclass called *Ver*. The Ver typeclass contains a function `ver` (`a -> Bool`) which verifies the integrity of its underlying value.

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
import AlgebraicStructures.Structures
import AlgebraicStructures.Base
import Data.Modular

n7 = [0..6] :: [Mod Integer 7]
g = Group (+) n7

isGroup = ver g -- True
```

The following structures are currently supported:
- Group-like: Magma, Semigroup, Monoid, Group, AbelianGroup, Semilattice
