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
n7 = [0..6] :: [Mod Integer 7]
ver $ Group (+) n7
-- True
```

The following properties and structures are currently supported:

**Properties**
- Closure
- Associativity
- Commutativity
- Identity
- Invertibility
- Idempotency
- Distributivity
- AbsorbingZero

**Group-like structures:**
- Magma
- Semigroup
- Monoid
- Group
- AbelianGroup
- Semilattice

**Ring-like structures:**
- Semiring
- Nearring
- Ring
- LieRing
- BoolRing
- Field

The qualities of these structures can easily be understood by looking at the source, which is why using a declarative language is useful here.

Take a ring for example:

```haskell
ver (Ring (+) (*) d) = all ver [AbelianGroup (+) d, Monoid (*) d] &&
                       all ver [Distributivity (+) (*) d, AbsorbingZero (+) (*) d]
```
