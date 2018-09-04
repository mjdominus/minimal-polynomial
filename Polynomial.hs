
module Polynomial where

-- A polynomial over some type of coefficients (a)
-- is just an assoc list of (power, coefficient) pairs
data Poly a = Poly [(Integer, a)]
zero = Poly []
