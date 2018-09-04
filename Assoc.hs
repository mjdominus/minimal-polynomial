
-- Assoc lists
module Assoc where

import Data.Maybe

-- maps key to values
data Assoc k v = Assoc [(k,v)] deriving Show
empty = Assoc []
isEmpty (Assoc []) = True
isEmpty _ = False

keys (Assoc as) = map fst as
vals (Assoc as) = map snd as
kvps (Assoc as) = as

-- apply a function to just the key
-- or to just the value
key f = f . fst
val f = f . snd

-- apply a function to just the key
-- or to just the value and return the resulting kvp
keym f = \(k, v) -> (f k,   v)
valm f = \(k, v) -> (  k, f v)

kvm f g = \(k, v) -> (f k, g v)

instance (Eq k, Eq v) => Eq (Assoc k v) where
  Assoc [] == Assoc []   = True
  Assoc _  == Assoc []   = False
  Assoc as == Assoc ((bk,bv):bs) =
    get_mb (Assoc as) bk == Just bv   &&
    ((Assoc as) `remove` bk) == (Assoc bs)

-- fmap maps over values, but not over keys
instance Functor (Assoc k) where
  fmap f (Assoc ls) = Assoc $ map (\(a,b) -> (a, f b)) ls

-- folds up values, not keys
instance Foldable (Assoc k) where
  foldMap f (Assoc as) = foldMap (val f) as

-- Assoc k v -> k -> Maybe v   (never fails)
(Assoc []) `get_mb` k = Nothing
(Assoc ((k',v):as)) `get_mb` k =
    if k == k' then Just v else (Assoc as) `get_mb` k

-- Assoc k v -> k -> v   (but might fail)
as `get` k =
  case as `get_mb` k of Nothing -> undefined
                        Just v  -> v

-- Assoc k v -> k -> Bool
as `has` k = isJust $ as `get_mb` k

-- Assoc k v -> k -> Assoc k v
-- (Assoc []) `remove` k = empty
remove (Assoc as) k = Assoc $ remove_ as k where
  remove_ [] _ = []
  remove_ ((k',v):as) k =
    if k == k' then rest
    else (k',v):rest
         where rest = remove_ as k

-- Assoc k v -> (k, v) -> Assoc k v
as `upd` (k,v) = Assoc $ (k,v):rest where
  Assoc rest = remove as k

-- lift a one-argument function, such as filter,
-- from lists to assocs
lift f x = fmap (f x)
-- the same, for two-argument functions
lift2 f x1 x2 = fmap (f x1 x2)

a1 = empty `upd` (1, "one")
a2 = a1 `upd` (2, "two")
a2' = empty `upd` (2, "two") `upd` (1, "one")
a2'' = a2 `upd` (1, "another one")

