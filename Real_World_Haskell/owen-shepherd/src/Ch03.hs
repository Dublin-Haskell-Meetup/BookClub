{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch03 where

import Prelude hiding (Left, Right)

import Control.Monad
import Data.Foldable
import Data.List hiding (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

len :: (Foldable f, Functor f) => f a -> Int
len = sum . fmap (const 1)

-- liable to overflow, should really use an iterative calculation ¯\_(ツ)_/¯
mean :: Fractional a => NonEmpty a -> a
mean = liftM2 (/) sum (fromIntegral . len)

palindrome :: [a] -> [a]
palindrome = ap (<>) reverse

isPalindrome :: Eq a => [a] -> Bool
isPalindrome = ap (==) reverse

sortByLen :: [[a]] -> [[a]]
sortByLen = sortOn len

intersperse :: a -> [[a]] -> [a]
intersperse a [] = []
intersperse a xs = tail $ xs >>= \e -> a : e

data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
    deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = succ $ max (height l) (height r)

data Direction
  = Straight
  | Left
  | Right
    deriving (Eq, Show)

newtype Point a = Point (a, a)
  deriving (Eq, Ord, Show)

add :: Num a => Point a -> Point a -> Point a
add (Point (x1, y1)) (Point (x2, y2)) = Point (x1 + x2, y1 + y2)

neg :: Num a => Point a -> Point a
neg (Point (x, y)) = Point (0 - x, 0 - y)

sub :: Num a => Point a -> Point a -> Point a
sub a b = add a (neg b)

ang :: RealFloat a => Point a -> Point a -> a
ang (Point (x1, y1)) (Point (x2, y2)) = atan2 (y2 - y1) (x2 - x1)

cross :: RealFloat a => Point a -> Point a -> a
cross (Point (x1, y1)) (Point (x2, y2)) = (x1 * y2) - (x2 * y1)

turn :: RealFloat a => Point a -> Point a -> Point a -> Direction
turn a b c =
  let
    v1 = sub b a
    v2 = sub c b
  in case compare (cross v1 v2) 0 of
    LT -> Right
    GT -> Left
    EQ -> Straight

turns :: RealFloat a => [Point a] -> [Direction]
turns (a : b : c : xs) = turn a b c : turns (b : c : xs)
turns _ = []

dotProd :: Num a => Point a -> Point a -> a
dotProd (Point (x1, y1)) (Point (x2, y2)) = x1 * x2 + y1 * y2

slope :: Fractional a => Point a -> Point a -> a
slope (Point (x1, y1)) (Point (x2, y2)) = (y2 - y1) / (x2 - x1)

swapPoint :: Point a -> Point a
swapPoint (Point (a, b)) = Point (b, a)

convexHull :: forall a. (Show a, RealFloat a) => [(Point a)] -> [(Point a)]
convexHull [] = []
convexHull l = let start = swapPoint $ minimum $ swapPoint <$> l
                   s = sortOn (ang start) $ filter (/= start) $ toList l
               in rec [start] s
  where
    rec :: [Point a] -> [Point a] -> [Point a]
    rec xs [] = xs
    rec (x : xs) (y : ys) | x == y = rec (x : xs) ys
    rec [x] (y : ys) = rec [y, x] ys
    rec axs@(x2 : x1 : xs) (y : ys)
          | turn x1 x2 y == Left = rec (y : axs) ys
          | otherwise = rec (x1 : xs) (y : ys)
