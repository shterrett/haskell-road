module Book.Hierarchy where

import Book.STAL(display)
import Book.SetEq

data S = Void deriving (Eq,Show)
empty :: Set S
empty = Set []

v0 = empty
v1 = powerSet v0
v2 = powerSet v1
v3 = powerSet v2
v4 = powerSet v3
v5 = powerSet v4
