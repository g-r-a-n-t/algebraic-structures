## Algebraic Structures

With this library you can create values that represent various algebraic structures and properties and validate them.

For example:
```haskell
import AlgebraicStructures.Structures
import AlgebraicStructures.Base
import Data.Modular

n7 = [0..6] :: Mod Integer 7
g = Group (+) n7

isGroup = ver g -- True
```

The following structures are currently supported:
- Group-like: Magma, Semigroup, Monoid, Group, AbelianGroup, Semilattice

