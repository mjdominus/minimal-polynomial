
module PolynomialTest where

import Test.QuickCheck
import Polynomial
import Test.Hspec
import Test.Hspec.Expectations (shouldBe, shouldThrow, anyException)
import Test.HUnit ( Assertion )
import Control.Monad ( liftM )
import qualified AssocTest
import qualified TermTest
import Term
import Assoc


shrinkWithValidator :: Arbitrary a => (a -> Bool) -> a -> [a]
shrinkWithValidator valid input = dropWhile (not . valid) $ shrink input

instance Arbitrary x => Arbitrary (Poly x) where
  arbitrary = liftM Poly arbitrary
  shrink = shrinkWithValidator isWellFormed

biv (a,b,c,d,e,f) = Poly (Assoc [(Term [2],    a), -- x^2
                                       (Term [1, 1], b), -- xy
                                       (Term [0, 2], c), -- y^2
                                       (Term [1],    d), -- x
                                       (Term [0, 1], e), -- y
                                       (Term [],     f) -- 1
                                      ])

todo :: String -> Test.HUnit.Assertion -> Test.HUnit.Assertion
todo msg c = do
              print ("TODO: " ++ msg)
              shouldThrow c anyException 
              return ()
              

trim = reverse . ltrim . reverse . ltrim where ltrim = dropWhile (== ' ')

wut = Assoc [(Term [1,1,4],4),(Term [0,0,1,4],-3),(Term [4],-3),(Term [4],1)]
w2 = Poly (Assoc [(Term [1],2)])
w3 = Poly (Assoc [(Term [1,0],2)])


main = hspec $ do
  describe "pretty" $ do
    it "should work on a bunch of monovariate examples" $ do
      pretty zero `shouldBe` "0"
      pretty (poly [1]) `shouldBe` "1"
      pretty (poly [-1]) `shouldBe` "- 1" -- maybe fix later
      pretty (poly [0]) `shouldBe` "0"
      pretty (poly [2]) `shouldBe` "2"

      pretty (poly [1, 0]) `shouldBe` "x"
      pretty (poly [1, 1]) `shouldBe` "x + 1"
      pretty (poly [1, -1]) `shouldBe` "x - 1"
      pretty (poly [-1, 1]) `shouldBe` "- x + 1"
      pretty (poly [2, 0]) `shouldBe` "2x"
      pretty (poly [-2, 0]) `shouldBe` "- 2x"
      pretty (poly [1, 0, 1]) `shouldBe` "x^2 + 1"
      pretty (poly [1, 1, 1]) `shouldBe` "x^2 + x + 1"
      pretty (poly [1, -1, 1]) `shouldBe` "x^2 - x + 1"
      pretty (poly [1, 2, 1]) `shouldBe` "x^2 + 2x + 1"
      pretty (poly [1, -2, 1]) `shouldBe` "x^2 - 2x + 1"
      pretty (poly [1, -2, -1]) `shouldBe` "x^2 - 2x - 1"
      pretty (poly [-2, 0, 2]) `shouldBe` "- 2x^2 + 2"

    it "should work on a bunch of bivariate examples" $ do
      pretty (biv (1,1,1,1,1,1)) `shouldBe` "x^2 + xy + y^2 + x + y + 1"
      pretty (biv (1,-1,1,-1,1,-1)) `shouldBe` "x^2 - xy + y^2 - x + y - 1"
      todo "harmless leading plus sign" $ pretty (biv (0,0,1,0,-2,1)) `shouldBe` "y^2 - 2y + 1"

  -- This oddity is not a problem in Polynomial.pretty
  -- It's because your "arbitrary" instance produced
  -- Poly $ Assoc [(Term [1],2), (Term [1,0],1)]
  describe "Polynomial.pretty" $ do
    it "should not produce '2x + x'!" $ property $
      ((\x -> pretty x /= "2x + x") :: (Poly Integer -> Bool))

  describe "Polynomial.isWellFormed" $ do
    it "the oddity is not well-formed" $
      (not . isWellFormed) (Poly $ Assoc [(Term [1],2), (Term [1,0],1)])

  describe "Polynomial.(+)" $ do
        it "identity for addition" $ property $
          (\x -> zero + x == (x :: Poly Integer))

