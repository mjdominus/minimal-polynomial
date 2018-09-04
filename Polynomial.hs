
module Polynomial where

-- A polynomial over some type of coefficients (a)
-- is just a (finite) sequence of coefficients, starting with the constant term
data Poly a = Poly [a]
zero = Poly []
coeff (Poly a) = a

-- the zero polynomial has multiple representations:
-- Poly []; Poly [0]; Poly [0,0]; etc.
isZero :: (Num a, Eq a) => Poly a -> Bool
isZero (Poly a) = all (== 0) a

-- Compare term-by-term, except that
-- if one polynomial is completely empty, just test
-- the other to see if it is zero
instance (Num a, Eq a) => Eq (Poly a) where
  (Poly []) == b         = isZero b
  a         == (Poly []) = isZero a
  (Poly (a:as)) == (Poly (b:bs)) =
     (a == b) && ((Poly as) == (Poly bs))

-- Wow, there sure are a lot of special cases!
instance (Eq a, Num a, Show a) => Show (Poly a) where
  show (Poly cs) = ltrim $ concat $ map concat terms where
    terms = case map show_a_term $          -- show each term
                 dropWhile ((== 0) . snd) $ -- discard leading zeroes
                 reverse $      -- largest power first
                 zip [0..] cs   -- adjoin power numbers
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

-- build a polynomial and trim off leading zero terms
-- YAGNI
poly cs = undefined

-- Multiply polynomial by x^n
shift n (Poly a) = Poly $ (take n $ repeat 0) ++ a

-- Multiply polynomial by scalar c
scale c (Poly a) = Poly $ (map (* c) a)

instance (Eq a, Num a) => (Num (Poly a)) where
  negate (Poly cs)     = Poly (map negate cs)
  abs = undefined
  signum = undefined
  fromInteger = undefined
  (Poly a) + (Poly b) = Poly $ zzipWith (+) a b where
    zzipWith _ []        bs  = bs
    zzipWith _    as     []  = as
    zzipWith o (a:as) (b:bs) = (o a b):zzipWith o as bs

  a * b = foldr (+) zero products where
    -- products :: [Poly a]
    products = map (`scaleBy` b) (termPairs a) where
      -- x^2 - 3 -> [(0,-3), (2,1)]
      termPairs (Poly a) = termPairs_ 0 a where
        termPairs_ _ [] = []
        termPairs_ p (0:cs) = termPairs_ (p+1) cs
        termPairs_ p (c:cs) = (p, c):termPairs_ (p+1) cs

      -- Multiply a monomial by a polynomial
      -- scaleBy (p, c) polynomial = polynomial * (cx^p)
      scaleBy (p, c) = (shift p) . (scale c)

-- collect :: Num a => [Poly a] -> Poly a
-- collect = zipWith (+)

instance Functor Poly where
  fmap f (Poly cs) = Poly $ fmap f cs

instance Applicative Poly where
  pure c = Poly [c]             -- constant polynomial
  (<*>) = undefined
--  (Poly ps) <*> (Poly cs) = collect $ Poly (

