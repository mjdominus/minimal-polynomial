
module Polynomial where
import Assoc
import Data.Align
import Data.List (sortOn)

-- A polynomial over some type of coefficients (a)
-- is just an assoc list of (power, coefficient) pairs
data Poly a = Poly (Assoc Integer a)
zero = Poly empty
assoc (Poly a) = a
p (Poly a) = a
sh :: Show a => Poly a -> String
sh = show . p

is_zero (Poly as) = all (key (== 0)) as

instance Functor Poly where
  fmap f (Poly a) = Poly $ fmap f a

-- This just delegates to the Assoc instance
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

-- degree of a polynomial
-- this depends on a normal form: no leading zeroes
degree (Poly as) = maximum $ keys as

-- return the coefficients of a polynomial
-- in *increasing* term order
coeffs p@(Poly as) = map (get0 as) [0 .. degree p] where
  get0 as = maybe 0 id . get_mb as

instance (Eq a, Num a, Show a) => Show (Poly a) where
  show (Poly as) = ltrim $ concat $ map concat terms where
    terms = case map show_a_term $          -- show each term
                 dropWhile ((== 0) . snd) $ -- discard leading zeroes
                 reverse $      -- largest power first
                 sortOn fst (kvps as) -- sorted pairs by power
            of
      [] -> [["0"]]             -- special special case
      -- otherwise delete leading plus sign
      (sgn:rest):more_terms ->
        if (sgn == plus) then rest:more_terms
        else                  (sgn:rest):more_terms
      anything_else -> anything_else

    -- show_a_term (power, coefficient), say (2, 3),
    -- gives back a list of things that look like
    --   ["+", "3", x^2"]
    show_a_term (_, 0)  = [                           ]
    show_a_term (0, c)  = [sgn c, show (abs c)        ]
    show_a_term (p, 1)  = [plus,                xpow p]
    show_a_term (p, -1) = [minus,               xpow p]
    show_a_term (p, c)  = [sgn c, show (abs c), xpow p]

    xpow 0 = ""                 -- not actually used
    xpow 1 = "x"
    xpow n = "x^" ++ (show n)

    sgn c = if (signum c) == 1 then plus
            else minus
    
    plus = " + "
    minus = " - "

    ltrim (' ':s) = ltrim s
    ltrim s = s

scale :: Num c => c -> Poly c -> Poly c
scale c = fmap (* c)

shift n (Poly a) = Poly $ kfmap (+ n) a

instance Align Poly where
  nil = zero
  align (Poly p1) (Poly p2) = Poly $ align p1 p2
  
instance (Eq a, Num a) => (Num (Poly a)) where
  negate     = fmap negate
  abs = undefined
  signum = undefined
  fromInteger = undefined
  (Poly a) + (Poly b) = Poly $ aZipWith (+) a b

  a * b = foldr (+) zero products where
    products = map (`scaleBy` b) (monomials a) where
      monomials (Poly a) = kvps a
      scaleBy (p, c) = (shift p) . (scale c)

p0 = poly [1]
p1 = poly [2]
p2 = poly [1, 0]
p3 = poly [1, 1]
p4 = poly [2, 3]
p5 = poly [1, 2, 1]
p6 = poly [-1]
p7 = poly [-1, 0]
p8 = poly [-2, 0]
p9 = poly [-2, -2]
pa = poly [1, 2, -1]
pb = poly [1, -2, 1]
pc = poly [-1, 2, 2]
pd = poly [-1, 0, -2]
pe = poly [3, 0, -1]
pf = poly [0, 3, 0, -1]
ps = [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9,
      pa, pb, pc, pd, pe, pf]

