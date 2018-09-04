
module Polynomial where
import Assoc
import Data.List (sort)


-- A polynomial over some type of coefficients (a)
-- is just an assoc list of (power, coefficient) pairs
data Poly a = Poly (Assoc Integer a)
zero = Poly empty
assoc (Poly a) = a

is_zero (Poly as) = all (key (== 0)) as

instance Functor Poly where
  fmap f (Poly a) = Poly $ fmap f a

instance Eq a => Eq (Poly a) where
  (Poly as) == (Poly bs)    = as == bs

-- take list of coefficients in **decreasing* power order
-- and build a polynomial
-- for example, takes [1, -6, 11, 6] and produces x^3 - 6x^2 + 11x + 6,
-- which is represented as 
-- Poly (Assoc [(0,6),(1,11),(2,-6),(3,1)])
poly coeffs = Poly (Assoc (filter ((/= 0) . snd) (index 0 (reverse coeffs)))) where
  index _ [] = []
  index n (a:as) = (n, a):(index (n+1) as)

