
-- Assoc lists
module Assoc where

import Data.Align
import Data.Maybe
import Data.These

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
  fmap f (Assoc ls) = Assoc $ map (valm f) ls

kfmap f (Assoc ls) = Assoc $ map (keym f) ls

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

as `getkvp` k = (k, as `get` k)

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

-- take a list of keys, and get the corresponding
-- values in the same order
gets keys as = map (get as) keys
-- or a list of (k, v) in the same order
getkvps :: Eq k => [k] -> Assoc k v -> [(k, v)]
getkvps keys as = map (getkvp as) keys

instance Eq k => Align (Assoc k) where
  nil = empty
  align as bs = Assoc $ realign merged_keys as bs  where
    merged_keys = merge (keys as) (keys bs)

    merge []     rs = rs
    merge (l:ls) rs = if l `elem` rs then rest else l:rest
      where rest = merge ls rs

    realign [] aa bb = []
    realign (k:ks) aa bb = let rest = realign ks aa bb in
        case (aa `get_mb` k, bb `get_mb` k) of
          (Nothing, Nothing) -> rest
          (Just a , Nothing) -> (k, This a):rest
          (Nothing, Just b)  -> (k, That b):rest
          (Just a, Just b)   -> (k, These a b): rest

-- Take two assocs, merge them into a single one, as follows:
-- For keys present in only one, keep the corresponding value
-- For keys present in both, combine them with f.          
aZipWith f as bs = fmap (zipThese f) (align as bs)
  where zipThese = these id id

a1 = empty `upd` (1, "one")
a2 = a1 `upd` (2, "two")
a2' = empty `upd` (2, "two") `upd` (1, "one")
a2'' = a2 `upd` (1, "another one")

fromLists ks vs = Assoc $ zip ks vs
