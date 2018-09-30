
-- Assoc lists
module Assoc where

import Data.Align
import Data.Maybe
import Data.These
import Data.List (nub)

-- maps key to values
data Assoc k v = Assoc [(k,v)] deriving Show
empty = Assoc []
isEmpty (Assoc []) = True
isEmpty _ = False

-- apply f to the list of pairs that underlies the assoc
lift f (Assoc as) = Assoc $ f as

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

-- This doesn't work properly in the presence of duplicate keys
instance (Eq k, Eq v) => Eq (Assoc k v) where
  Assoc [] == Assoc []   = True
  Assoc _  == Assoc []   = False
  Assoc as == Assoc ((bk,bv):bs) =
    get_mb (Assoc as) bk == Just bv   &&
    ((Assoc as) `remove` bk) == (Assoc bs)

-- Might be easier to use Align?
-- (I don't even know what align would do in the presence of duplicate keys)
-- instance (Eq k, Eq v) => Eq (Assoc k v) where
--   as == bs = foldr equa_l True (vals $ align as bs) where
--     equa_l (These p q) True  = p == q
--     equa_l _           _     = False

-- fmap maps over values, but not over keys
instance Functor (Assoc k) where
  fmap f = lift $ map (valm f)

-- This transforms keys instead, leaving values the same
kfmap :: (t -> k) -> Assoc t v -> Assoc k v
kfmap f = lift $ map (keym f)

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
assoc `remove` k = lift (`remove_` k) assoc where
  remove_ [] _ = []
  remove_ ((k',v):as) k =
    if k == k' then rest
    else (k',v):rest
         where rest = remove_ as k

-- Assoc k v -> (k, v) -> Assoc k v
as `upd` (k,v) =
  Assoc $ (k,v):rest where
    Assoc rest = remove as k
-- Why didn't I just use
as `upd'` pair = lift (pair :) as
-- Maybe for easier debugging during development?

-- This version is broken.  Can QuickCheck figure that out?
-- if (length rest == 4) then empty else Assoc $ (k,v):rest where
-- Yes, it totally did.

-- take a list of keys, and get the corresponding
-- values in the same order
gets keys as = map (get as) keys
-- or a list of (k, v) in the same order
getkvps :: Eq k => [k] -> Assoc k v -> [(k, v)]
getkvps keys as = map (getkvp as) keys

-- 
instance Eq k => Align (Assoc k) where
  nil = empty
  align as bs = Assoc $ realign merged_keys as bs  where
    merged_keys = nub $ (keys as) ++ (keys bs)

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
