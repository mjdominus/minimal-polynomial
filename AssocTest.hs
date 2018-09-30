module AssocTest where

-- import Test.Hspec
import Test.QuickCheck
import Assoc
import Data.Maybe

emptyProp :: Eq k => k -> Bool
emptyProp k = isNothing $ empty `get_mb` k

putgetProp :: (Assoc String String, String, String) -> Bool
putgetProp (as, k, v) = as `upd` (k, v) `get` k == v

putgetmbProp :: (Eq k, Eq v) => (Assoc k v, k, v) -> Bool
putgetmbProp (as, k, v) = let res = as `upd` (k, v) `get_mb` k
                              in isJust res && res == Just v

hasProp :: (Eq k, Eq v) => Assoc k v -> (k, v) -> Bool
hasProp as (k, v) = (as `upd` (k,v)) `has` k

missingKey :: Assoc Int v -> Int
missingKey as = 1 + (maximum $ keys as)

hasProp2 :: Assoc Int v -> Bool
hasProp2 as = not $ (as `has` (missingKey as))

getMissingProp :: Assoc Int v -> Bool
getMissingProp as = isNothing $ as `get_mb` (missingKey as)

-- After we remove a key from an assoc, is it really gone?
-- Your initial implementation didn't do this;
-- it assumed that each key appeared only once!
removeProp (as, k) = isNothing $ (as `remove` k) `get_mb` k

-- Are these two implementations of "update" equivalent?
-- Not at present, because == doesn't understand about duplicate key semantics
-- (Ignore all but the first)
updatesProp (as, k, v) = (as `upd` (k, v)) == (as `upd'` (k, v))

instance (Arbitrary k, Arbitrary v) => Arbitrary (Assoc k v)  where
    arbitrary = sized arbAssoc where
      arbAssoc 0 = return empty
      arbAssoc n = do Assoc aList <- arbAssoc (n-1)
                      newKVP <- arbitrary
                      return $ Assoc $ (newKVP : aList)
    shrink (Assoc ls) = map Assoc (shrink ls)

-- main :: IO ()
main = do
  quickCheck ((\k -> isNothing $ empty `get_mb` k) :: String -> Bool)
  quickCheck ((\k -> isNothing $ empty `get_mb` k) :: Int -> Bool)
  quickCheck (putgetmbProp :: (Assoc String String, String, String) -> Bool)
  quickCheck (putgetmbProp :: (Assoc Int Int, Int, Int) -> Bool)
  quickCheck (hasProp :: Assoc Int Int -> (Int, Int) -> Bool)
  quickCheck (hasProp2 :: Assoc Int String -> Bool)
  quickCheck (getMissingProp :: Assoc Int String -> Bool)
  quickCheck (removeProp :: (Assoc String String, String) -> Bool)
  quickCheck (removeProp :: (Assoc Integer Integer, Integer) -> Bool)
-- See above
--  quickCheck (updatesProp :: (Assoc String String, String, String) -> Bool)

-- main = hspec $ do
--   describe "empty assoc" $ do
--     it "getmb always returns None" $ property $ propEmpty

--     -- it "returns a positive number when given a negative input" $
--     --   absolute (-1) `shouldBe` 1

--     -- it "returns zero when given zero" $
--     --   absolute 0 `shouldBe` 0
