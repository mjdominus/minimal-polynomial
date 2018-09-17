
module Term where

-- A coefficient-less term like [2, 3] means x^2 * y^3
newtype Term = Term [Integer]

normalize (Term t) = Term $ reverse . (dropWhile (== 0)) . reverse $ t

instance Eq Term where
  s == t     = (pows . normalize) s == (pows . normalize) t

-- this is going to need some adjustments for special cases
instance Show Term where
  show (Term []) = ""
  show (Term t) = show_ "xyztw" t where
    show_ (v:vs) []     = ""
    show_ (v:vs) (0:ps) = show_ vs ps
    show_ (v:vs) (1:ps) = [v] ++ show_ vs ps
    show_ (v:vs) (p:ps) = [v] ++ "^" ++ (show p) ++ show_ vs ps

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
      
