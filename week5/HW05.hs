{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

import Data.Bits (xor)
import qualified Parser as P
import qualified Data.Aeson as A
import Data.List (sort)

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret p1 p2 = do
    f1 <- BS.readFile p1
    f2 <- BS.readFile p2
    return . BS.filter ((/=) (fromInteger 0)) . BS.pack $ BS.zipWith (xor) f1 f2

-- Exercise 2 -----------------------------------------

getKey :: IO ByteString
getKey = getSecret "clues/dog.jpg" "clues/dog-original.jpg"

newName :: FilePath -> FilePath
newName = reverse . stripHead (reverse ".enc") . reverse
    where stripHead [] bs = bs
          stripHead _ [] = []
          stripHead (a:as) (b:bs)
            | a == b = stripHead as bs
            | otherwise = error "bad substring"

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k p = do
    let op = newName p
    f1 <- BS.readFile p
    putStrLn $ "read " ++ show (BS.length f1)
    putStrLn $ "decrypting with " ++ ( show k)
    putStrLn $ "writing results to " ++ op
    BS.writeFile op . BS.pack $ BS.zipWith (xor) f1 (BS.cycle k)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile p = do
    f <- BS.readFile p
    return $ A.decode f

-- Exercise 4 -----------------------------------------

filterTs :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
filterTs Nothing _ = error "No TIds"
filterTs (Just _) Nothing = error "no transactions"
filterTs (Just ids) (Just ts) = Just $ filter (\t-> (tid t) `elem` ids) ts

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vp tp = do
    mv <- parseFile vp :: IO (Maybe [TId])
    mt <- parseFile tp :: IO (Maybe [Transaction])
    return $ filterTs mv mt

-- Exercise 5 -----------------------------------------

flowOp :: Map String Integer -> Transaction -> Map String Integer
flowOp e t
    | Map.member (from t) e, Map.member (to t) e = Map.adjust ((+) (amount t)) (to t) $ Map.adjust (flip(-) (amount t)) (from t) e
    | Map.notMember (from t) e, Map.member (to t) e=
       Map.adjust ((+) (amount t)) (to t) . Map.adjust (flip(-) (amount t)) (from t) $ Map.insert (from t) 0 e
    | Map.notMember (to t) e, Map.member (from t) e=
       Map.adjust (+ (amount t)) (to t) . Map.adjust (flip (-) (amount t)) (from t) $ Map.insert (to t) 0 e
    -- | Map.notMember (to t) e, Map.notMember (from t) e=
    | otherwise =
       Map.adjust (+ (amount t)) (to t) . Map.adjust (flip (-) (amount t)) (from t) . Map.insert (from t) 0 $ Map.insert (to t) 0 e

getFlow :: [Transaction] -> Map String Integer
getFlow = foldl flowOp Map.empty 

testTs1 = [
    Transaction { from = "Haskell Curry"
                , to   = "Simon Peyton Jones"
                , amount = 10
                , tid  = "1234"
                }
    ]
testTs2 = [
    Transaction { from = "Haskell Curry"
                , to   = "Simon Peyton Jones"
                , amount = 10
                , tid  = "1234"
                }
    ,Transaction { from = "Simon Peyton Jones"
                , to   = "Haskell Curry"
                , amount = 5
                , tid  = "1234"
                }
                ]
-- Exercise 6 -----------------------------------------

crimHelp m = sort $ [ (a, k) | (k, a) <- Map.toList m]

getCriminal :: Map String Integer -> String
getCriminal m = snd . head . reverse $ crimHelp m

-- Exercise 7 -----------------------------------------

justice :: [(Integer, String)] -> [(Integer, String)] -> [TId] -> [Transaction]
justice [] (_:_) _ = error "Paid off all debts but still have bad money"
justice (_:_) [] _ = error "Not enough money to pay off the depts"
justice [] [] _ = []
justice ((ra,rn):rs) ((ba,bn):bs) (t:ts) 
    | abs ra < ba =
      Transaction {from=bn,to=rn,amount=(-ra),tid=t}:justice rs ((ba+ra,bn):bs) ts
    | abs ra > ba =
      Transaction {from=bn, to=rn, amount=(ba), tid=t}:justice ((ra+ba,rn):rs) bs ts
    | abs ra == ba =
      Transaction {from=bn, to=rn, amount=ba, tid=t}:justice rs bs ts

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ts = justice rs bs ts
    where rs = reverse . filter ((< 0) . fst) $ crimHelp m
          bs = reverse . filter ((> 0) . fst) $ crimHelp m

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON p d = do
    let bs = A.encode d
    BS.writeFile p bs

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  putStrLn vict
  decryptWithKey key vict
  mts <- getBadTs (newName vict) trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

