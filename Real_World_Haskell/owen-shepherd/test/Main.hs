module Main where

import Test.Tasty

import TestCh03

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ch03]
