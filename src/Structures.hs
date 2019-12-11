module Structures () where

import Base

data Group a = Group [a] | AbelianGroup [a]

newGroup :: (Eq a, Add a) => [a] -> Group a
newGroup d = Group d

