module TestCh03 ( ch03 ) where

import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import Ch03 as Ch03

listRange :: Range Int
listRange = (Range.linear 0 20)

genlist :: Gen a -> Gen [a]
genlist = Gen.list listRange

genNE :: Gen a -> Gen (NE.NonEmpty a)
genNE = Gen.nonEmpty listRange

genfloat :: Gen Float
genfloat = Gen.float (Range.linearFrac (-100) 100)

lenIsLength :: Property
lenIsLength = property $ do
  xs <- forAll $ genlist Gen.alpha
  length xs === len xs

meanGtMin :: Property
meanGtMin = property $ do
  xs <- forAll $ genNE genfloat
  HH.assert $ mean xs >= minimum xs

meanLtMax :: Property
meanLtMax = property $ do
  xs <- forAll $ genNE genfloat
  HH.assert $ mean xs <= maximum xs

palindromesArePalindromes :: Property
palindromesArePalindromes = property $ do
  xs <- forAll $ genlist Gen.alpha
  HH.assert $ isPalindrome $ palindrome xs

sortByLenMin :: Property
sortByLenMin = property $ do
  xs <- forAll $ genNE $ genlist Gen.alpha
  len (head (sortByLen $ NE.toList xs)) === minimum (fmap len xs)

sortByLenMax :: Property
sortByLenMax = property $ do
  xs <- forAll $ genNE $ genlist Gen.alpha
  len (head (reverse $ sortByLen $ NE.toList xs)) === maximum (fmap len xs)

intersperseProp :: Property
intersperseProp = property $ do
  xs <- forAll $ genlist $ genlist Gen.alpha
  Ch03.intersperse ',' xs === intercalate "," xs

pts :: RealFloat a => [(a, a)] -> [(Point a)]
pts = fmap Point

ch03 :: TestTree
ch03 = testGroup "Ch03"
  [ testGroup "len"
    [ testProperty "len == length" lenIsLength
    ]
  , testGroup "mean"
    [ testProperty "mean <= max" meanGtMin
    , testProperty "mean >= min" meanLtMax
    ]
  , testGroup "palindrome"
    [ testCase "palindrome \"\"" $ palindrome "" @?= ""
    , testCase "palindrome \"hi\"" $ palindrome "hi" @?= "hiih"
    ]
  , testGroup "isPalindrome"
    [ testProperty "palindromes are palindromes" $ palindromesArePalindromes
    ]
  , testGroup "sortByLen"
    [ testProperty "sortByLen min" $ palindromesArePalindromes
    , testProperty "sortByLen max" $ palindromesArePalindromes
    ]
  , testGroup "intersperse"
    [ testProperty "intersperse == Prelude.intersperse" intersperseProp
    ]
  , testGroup "convex hull"
    [ testCase "convex hull [(0,0)]" $
      convexHull (pts [(0,0)]) @?= pts [(0,0)]
    , testCase "convex hull 0" $
      convexHull (pts [(0,0), (1, 0)])
              @?= pts [(1, 0), (0, 0)]
    , testCase "convex hull 1" $
      convexHull (pts [(0,0), (1, 0), (1, 1)])
              @?= pts [(1, 1), (1, 0), (0, 0)]
    , testCase "convex hull 2" $
      sort (convexHull (pts [(0,0), (1, 0), (1, 1), (0, 1)]))
              @?= pts (sort [(1, 1), (1, 0), (0, 0), (0, 1)])
    , testCase "convex hull 3" $
      sort (convexHull (pts [(0,0), (1, 0), (1, 1), (0.5, 0.5), (0, 1)]))
              @?= pts (sort [(1, 1), (1, 0), (0, 0), (0, 1)])
    ]
  ]
