module TermTest where

-- import Test.Hspec
import Test.QuickCheck
import Term
import Control.Monad ( replicateM )
import Test.Hspec

instance Arbitrary Term where
  arbitrary = do
    nvars <- choose (1, 4)
    pows <- replicateM nvars (arbitrary `suchThat` (>= 0))
    return $ Term pows
  shrink (Term ls) = map Term (shrink ls)

-- normProp (Term []) 
normProp t@(Term ls) =
  (any (/= 0) ls) ==> 
  (((last ls) == 0) == (t /= (normalize t)))
  
main = hspec $ do
  describe "Term.pows" $ do
    it "should invert Term" $ property $
      (\x -> (pows . Term) x == x)

  describe "Term.degree" $ do
    it "should work on some examples" $ do
      degree (Term []) `shouldBe` 0
      degree (mono 3) `shouldBe` 3
      degree (mono 1) `shouldBe` 1
      degree (mono 0) `shouldBe` 0
      degree (Term [0, 7]) `shouldBe` 7
      degree (Term [3, 4]) `shouldBe` 7
      degree (Term [3, 0, 0, 4]) `shouldBe` 7
        
  describe "Term.str" $ do
    it "should do what I expect" $ do
      str (mono 0) `shouldBe` "" -- special case
      str (mono 1) `shouldBe` "x"
      str (mono 2) `shouldBe` "x^2"
      str (mono 3) `shouldBe` "x^3"
      str (Term [0, 0]) `shouldBe` ""
      str (Term [1, 0]) `shouldBe` "x"
      str (Term [0, 1]) `shouldBe` "y"
      str (Term [1, 1]) `shouldBe` "xy"
      str (Term [2, 0]) `shouldBe` "x^2"
      str (Term [0, 2]) `shouldBe` "y^2"
      str (Term [3, 2]) `shouldBe` "x^3y^2"
      str (Term [3, 0, 1]) `shouldBe` "x^3z"

  describe "Term.mul" $ do
    it "should multiply monomials" $ property $
      (\(a, b) -> (Term [a]) `mul` (Term [b]) == (Term [a+b]))

    it "should multiply powers of different variables" $ property $
      (\(a, b) -> (Term [a]) `mul` (Term [0, b]) == (Term [a, b]))

    it "should multiply powers of different variables" $ property $
      (\(a, b) -> (Term [0,0,a]) `mul` (Term [0, b]) == (Term [0, b, a]))

    it "should work on some examples" $
      Term [ 1, 2 ] `mul` Term [0, 2, 1] `shouldBe` Term [1, 4, 1]

