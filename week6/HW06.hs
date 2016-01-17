{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x s) = [x] ++ streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x s) = Cons (f x) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
--sInterleave (Cons _ _) _ = undefined
sInterleave (Cons x s1) s2 = Cons x (sInterleave s2 s1)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake i (Cons x s) = x:(sTake (i-1) s)

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = natsHelper 0
       where natsHelper i = Cons i (natsHelper (i+1))

rulerDumb :: Stream Integer
rulerDumb = rulerHelper 1
        where powersOfTwo x =[ i | i <- [1..x], (2^i) <= x]
              lp2 x = maximum (0:[j | j <- powersOfTwo x , x `mod` (2^j) == 0])
              rulerHelper i = Cons (lp2 i) (rulerHelper (i+1))

ruler :: Stream Integer
ruler = foldr (\sd st-> sInterleave (sRepeat sd) st) (sRepeat 0) [0..]

-- Exercise 7 -----------------------------------------
randHelp :: Int -> Stream Int
randHelp p = Cons n (randHelp n)
   where n = (1103515245 * p + 12345) `mod` 2147483648

-- | Implementation of C rand
rand :: Int -> Stream Int
rand i = randHelp i
             

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 221 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax is = Just $ foldl' (\(n,x) v-> n `seq` x `seq`  (min n v, max x v)) (maxBound :: Int, minBound :: Int) is

minMax2 :: [Int] -> Maybe (Int, Int)
minMax2 [] = Nothing
minMax2 vs = Just $ mM (maxBound::Int,minBound::Int) vs

mM :: (Int, Int) -> [Int] -> (Int, Int)
mM nx [] = nx
mM (cn,cx) (v:vs) = cn `seq` cx `seq` mM (min cn v, max cx v) vs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
