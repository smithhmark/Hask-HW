{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List (sort, groupBy, sortBy)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P (_:_) == P [] = False
    P [] == P (_:_) = False
    P [] == P [] = False -- ???
    P (a:as) == P (b:bs)
      | a == b, length as == length bs = P as ==  P bs
      | otherwise = False
 
-- Exercise 3 -----------------------------------------

labelTerms :: Poly a -> [(Int, a)]
labelTerms (P a) = zip [0,1..] a

termify :: (Show a, Eq a, Num a) => [(Int, a)] -> [String]
termify [] = []
termify ((d,c):ts)
  | fromInteger 0 == c = termify ts
  | d == 0 = (show c):termify ts
  | d == 1, fromInteger 1 == c = "x":termify ts
  | d == 1 = (show c ++ "x"):termify ts
  | otherwise = (
    (if fromInteger 1 == c then "" else show c) ++ "x^" ++ show d):termify ts

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [0]) = "0"
    show p = foldr1 (\t e->t++" + "++e) . reverse . termify $ labelTerms p

-- Exercise 4 -----------------------------------------

plusHelper :: (Num a) => [a] -> [a] -> [a]
plusHelper [] bs = bs
plusHelper as [] = as
plusHelper (a:as) (b:bs) = a + b:plusHelper as bs

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P as) (P []) = P as
plus (P []) (P bs) = P bs
plus (P as) (P bs) = P $ plusHelper as bs

-- Exercise 5 -----------------------------------------

bit :: Num a => [(Int, a)] -> [(Int, a)] -> [[(Int, a)]]
bit as bs = foldr (\ (ad,ac) e->[((ad+bd),(ac*bc)) | (bd,bc) <- bs]:e) [] as

times :: (Num a) =>  Poly a -> Poly a -> Poly a
times (P as) (P bs) = foldr (+) (P [0]) . map (polytize) $ bit as' bs' 
    where as' = zip [0..] as
          bs' = zip [0..] bs
  
polytize :: (Num a) => [(Int, a)] -> Poly a
polytize cs = P $ [snd t | t <- ts]
    where scs = sortBy (\a b->compare (fst a) (fst b)) cs
          dss = groupBy (\a b->(fst a) == (fst b)) scs
          cs' = map (foldr1 (\ (_,c) (d,e)->(d,c+e))) dss
          ts = op cs' [(b, 0) | b <- [0..]]
          op [] _ = []
          op allAs@(a:as) (b:bs)
             | fst a == fst b = a:op as bs
             | otherwise = b:op allAs bs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate  (P as)  = P $ map ((*) (-1)) as
    fromInteger a = P (fromInteger a:[])
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P as) v =  foldr (\ (bd, bc) e->v^bd * bc + e) 0 $ zip [0..] as

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 p = p
    nderiv n p = nderiv (n-1) (deriv p)

-- Exercise 9 -----------------------------------------

derivOp :: (Num a) => Integer -> [a] -> [a]
derivOp _ [] = []
derivOp d (t:ts)
  | d == 0 = derivOp (d+1) ts
  | otherwise = (fromInteger d)*t:derivOp (d+1) ts

instance Num a => Differentiable (Poly a) where
    deriv (P ts) = P $ derivOp 0 ts

