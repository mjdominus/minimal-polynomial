
module Polynomial where
import Assoc
import Data.Align
import Data.List (sortOn, sort)
import qualified Term 

-- A polynomial over some type of coefficients (a)
-- is just an assoc list of (power, coefficient) pairs
data Poly a = Poly (Assoc Term.Term a)
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
-- and build a univariate polynomial
-- for example, takes [1, -6, 11, 6] and produces x^3 - 6x^2 + 11x + 6,
-- which is represented as 
-- Poly (Assoc [(0,6),(1,11),(2,-6),(3,1)])
poly coeffs = Poly (Assoc $
                    map (keym Term.mono)
                    (filter ((/= 0) . snd) (index 0 (reverse coeffs)))) where
  index _ [] = []
  index n (a:as) = (n, a):(index (n+1) as)

-- degree of a polynomial
-- this depends on a normal form: no leading zeroes
-- fails when applied to zero polynomial; fix this if needed
degree (Poly as) = maximum $ map Term.degree $ keys as

terms (Poly a) = sort $ keys a

isWellFormed (Poly a) = none hasTrailingZeroes (keys a) where
  none f = not . any f
  hasTrailingZeroes (Term.Term []) = False
  hasTrailingZeroes (Term.Term ls) = head (reverse ls) == 0
  
-- return the coefficients of a polynomial
-- in *increasing* term order
coeffs p@(Poly as) = map (get as) (terms p)

p1 = poly [1]                   -- 1
p2 = poly [1, 2]                -- 2 + x
p3 = poly [1, 2, 1]             -- 1 + 2x + x^2

-- xy + x + y + 1
-- bleh, we need a better notation
p4 = Poly $ Assoc [(Term.Term [1,1], 1), (Term.Term [0, 1], 1), (Term.Term [1, 0], 1), (Term.Term [0, 0], 1)]

pretty (Poly as) = ltrim $ concat $ map concat term_strings where
  term_strings = case map show_a_term $ sorted_normalized_terms as  of
    [] -> [["0"]]             -- special special case
          -- otherwise delete leading plus sign
    (sgn:rest):more_terms ->
        if (sgn == plus) then rest:more_terms
        else                  (sgn:rest):more_terms
    anything_else -> anything_else
  sorted_normalized_terms (Assoc pairs) =
    sortOn fst $ map norm_term $ pairs   where
      norm_term (t, c) = (Term.normalize t, c)

    -- show_a_term (term, coefficient), say (2, 3),
    -- gives back a list of things that look like
    --   ["+", "3", x^2"]
  show_a_term (_, 0)  = [                           ]
  show_a_term (Term.Term [], c)
                      = [sgn c, show (abs c)        ]
  show_a_term (t, 1)  = [plus,                Term.str t]
  show_a_term (t, -1) = [minus,               Term.str t]
  show_a_term (t, c)  = [sgn c, show (abs c), Term.str t]

  sgn c = if (signum c) == 1 then plus
          else minus
    
  plus = " + "
  minus = " - "

  ltrim (' ':s) = ltrim s
  ltrim s = s

instance (Eq a, Num a, Show a) => Show (Poly a) where
  show = sh

scale :: Num c => c -> Poly c -> Poly c
scale c = fmap (* c)

instance Align Poly where
  nil = zero
  align (Poly p1) (Poly p2) = Poly $ align p1 p2
  
instance (Eq a, Num a) => (Num (Poly a)) where
  negate     = fmap negate
  abs = undefined
  signum = undefined
  fromInteger = undefined
  (Poly a) + (Poly b) = Poly $ aZipWith (+) a b
  (*) = undefined

--   a * b = foldr (+) zero products where
--     products = map (`scaleBy` b) (monomials a) where
--       monomials (Poly a) = kvps a
--       scaleBy (p, c) = (shift p) . (scale c)

-- p0 = poly [1]
-- p1 = poly [2]
-- p2 = poly [1, 0]
-- p3 = poly [1, 1]
-- p4 = poly [2, 3]
-- p5 = poly [1, 2, 1]
-- p6 = poly [-1]
-- p7 = poly [-1, 0]
-- p8 = poly [-2, 0]
-- p9 = poly [-2, -2]
-- pa = poly [1, 2, -1]
-- pb = poly [1, -2, 1]
-- pc = poly [-1, 2, 2]
-- pd = poly [-1, 0, -2]
-- pe = poly [3, 0, -1]
-- pf = poly [0, 3, 0, -1]
-- ps = [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9,
--       pa, pb, pc, pd, pe, pf]

