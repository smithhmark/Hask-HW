{-# OPTIONS_GHC -Wall #-}
module HW02 where

import qualified Data.List as L

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = foldr op 0 $ zip xs ys
                    where op (a,b) c = if a == b then c + 1 else c

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors ps = map sum . map (map snd) . L.groupBy comp . L.sort $ foldr op (zip colors $ repeat 0) ps
             where comp (a,_) (b,_) = a==b
                   op p e =  (p,1):e

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum . map (uncurry min) $ zip (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

inexactMatches :: Code -> Code -> Int
inexactMatches s g = matches remsec remgues
                     where lessexact = filter (uncurry (/=)) $ zip s g 
                           remsec = map fst lessexact
                           remgues = map snd lessexact

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exact inexact
              where exact = exactMatches s g
                    inexact = inexactMatches s g

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move g e i) c
  | e == exactMatches c g, i == inexactMatches c g = True
  | otherwise  = False

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes g = filter (isConsistent g)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [p:cs | p <- colors, cs <- allCodes (n-1)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve s = guesswork (allCodes (L.length s)) s []

guesswork :: [Code] -> Code -> [Move] -> [Move]
guesswork (c:cs) s [] = guesswork (c:cs) s [getMove s c]
guesswork cs s (mv@(Move g e i):[])
  | e == length s = mv:[]
  | e > 0 || i > 0 = let ncs = filter (isConsistent mv) cs
                         ng = head $ filter (not . (`elem` g:[])) ncs
                      in guesswork ncs s (getMove s ng:mv:[])
  | otherwise = let ncs = removeColors (colorsUsed g) cs
                    nm = head ncs
                in guesswork ncs s (getMove s nm:mv:[])
guesswork cs s (mv@(Move g e i):mvs)
  | e == length s = mv:mvs
  | e > 0 || i > 0 = let ncs = filter (isConsistent mv) cs
                         gs = g:foldr (\ (Move gu _ _) acc-> gu:acc) [] mvs
                         ng = head $ filter (not . (`elem` gs)) ncs
                      in guesswork ncs s (getMove s ng:mv:mvs)
  | otherwise = let ncs = removeColors (colorsUsed g) cs
                    nm = head ncs
                in guesswork ncs s (getMove s nm:mv:mvs)

colorsUsed :: Code -> [Peg]
colorsUsed = map head . L.groupBy (==) . L.sort 

removeColors :: [Peg] -> [Code] -> [Code]
removeColors ps cs = filter (and . map (not . (`elem` ps))) cs

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
