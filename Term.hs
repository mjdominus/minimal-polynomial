
module Term where

-- A coefficient-less term like [2, 3] means x^2 * y^3
newtype Term = Term [Integer] deriving Show

-- apply f to the list of powers that underlies the term
lift f (Term t) = Term $ f t

normalize = lift (reverse . (dropWhile (== 0)) . reverse)
term  = normalize . Term

instance Eq Term where
  s == t     = (pows . normalize) s == (pows . normalize) t

-- this is going to need some adjustments for special cases
str (Term []) = ""
str (Term t) = str_ "xyztw" t where
    str_ (v:vs) []     = ""
    str_ (v:vs) (0:ps) = str_ vs ps
    str_ (v:vs) (1:ps) = [v] ++ str_ vs ps
    str_ (v:vs) (p:ps) = [v] ++ (map sup $ show p) ++ str_ vs ps where
      sup '0' = '⁰'
      sup '1' = '¹'
      sup '2' = '²'
      sup '3' = '³'
      sup '4' = '⁴'
      sup '5' = '⁵'
      sup '6' = '⁶'
      sup '7' = '⁷'
      sup '8' = '⁸'
      sup '9' = '⁹'

-- x^p
mono p = Term [p]
pows (Term ps) = ps

degree (Term t) = sum t

instance Ord Term where
  compare s t =
    case (degree s) `compare` (degree t) of
      EQ -> (pows t) `compare` (pows s) -- reversed so x comes before y
  -- reversed so that high degrees come before low degrees
      GT -> LT
      LT -> GT

-- # Multiply  two terms to make a new term
-- #  For example Term [1, 2]   ( xy^2 )
-- #               mul
-- #              Term [2, 0, 1]   (x^2z)
-- # = Term [3, 2, 1]
mul (Term a) (Term b) =
  term $ softZipWith (+) a b where
    softZipWith _ [] bs = bs
    softZipWith _ as [] = as
    softZipWith f (a:as) (b:bs) = (f a b) : softZipWith f as bs


