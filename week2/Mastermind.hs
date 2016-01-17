module Mastermind where

import qualified Data.List as L

data Peg = Red | Orange | Blue | Green | Yellow | Purple deriving (Show, Eq, Ord)

type Code = [Peg]

colors = [Red,Orange,Blue,Green,Yellow,Purple]

exactMatches :: Code -> Code -> Int
exactMatches xs ys = foldr op 0 $ zip xs ys
                    where op (a,b) c = if a == b then c + 1 else c

countColors :: Code -> [Int]
countColors ps = map sum . map (map snd) . L.groupBy comp . L.sort $ foldr op (zip colors $ repeat 0) ps
             where comp (a,_) (b,_) = a==b
                   op p e =  (p,1):e

matches :: Code -> Code -> Int
matches c1 c2 = sum . map (uncurry min) $ zip (countColors c1) (countColors c2)


