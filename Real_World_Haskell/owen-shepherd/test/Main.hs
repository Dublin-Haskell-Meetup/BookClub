module Main where

import Test.Tasty

import TestCh03
import TestCh05

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ ch03
  , ch05
  ]
