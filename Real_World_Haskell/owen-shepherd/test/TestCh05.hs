module TestCh05 ( ch05 ) where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit

import Ch05

val :: JValue
val = JObject [("f", JNumber 1), ("q", JBool True)]

res :: String
res = intercalate "\n"
  [ "{\"f\": 1.0,"
  , "\"q\": true"
  , "}"
  ]

ch05 :: TestTree
ch05 = testGroup "Ch05"
  [ testCase "render json" $ pretty 10 (renderJValue val) @?= res
  ]
