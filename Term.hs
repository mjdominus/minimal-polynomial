
module Term where

-- A coefficient-less term like [2, 3] means x^2 * y^3
newtype Term = Term [Int]

-- Just trim the trailing zeroes
normalize (Term t) = Term $ reverse . (dropWhile (== 0)) . reverse $ t

-- this is going to need some adjustments for special cases
instance Show Term where
  show (Term t) = show_ "xyztw" t of ""
    show_ (v:vs) []     = ""
    show_ (v:vs) (0:ps) = show_ vs ps
    show_ (v:vs) (1:ps) = [v] ++ show_ vs ps
    show_ (v:vs) (p:ps) = [v] ++ "^" ++ (show p) ++ show_ vs ps
    
    
    
    
    
  

