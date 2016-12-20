module Firewall where

import           Control.Monad.State
import           Data.List           (foldl')
import           Text.Trifecta

testInput :: String
testInput = "5-8\n0-1\n3-4"

newtype Range = Range (Integer, Integer)
    deriving (Show, Eq, Ord)

rangeParser :: Parser Range
rangeParser = do
    x <- decimal
    _ <- char '-'
    y <- decimal
    return $ Range (x, y)

parseInput :: String -> Result [Range]
parseInput s = traverse parseLine (lines s)

parseLine :: String -> Result Range
parseLine = parseString rangeParser mempty

addRange :: Range -> [Range] -> [Range]
addRange r [] = [r]
addRange r@(Range (x1, y1)) b@((Range (x2, y2)) : rs) =
    case compare x1 x2 of
        LT -> if y1 < x2 - 1
              then r : b
              else addRange (Range (x1, maximum [y1, y2])) rs
        EQ -> addRange (Range (x1, maximum [y1, y2])) rs
        GT -> if x1 <= y2 + 1
              then addRange (Range (x2, maximum [y1, y2])) rs
              else Range (x2, y2) : addRange r rs

combineRanges :: [Range] -> [Range]
combineRanges = foldr addRange []

runTest :: [Range]
runTest = case parseInput testInput of
    Success rs -> combineRanges rs
    Failure x  -> error (show x)

findMin :: [Range] -> Integer
findMin (Range (_, m) : _) =
    m + 1

partOne :: String -> Integer
partOne s = case parseInput s of
    Success rs -> findMin $ combineRanges rs
    Failure x  -> error (show x)

partTwo :: String -> Integer
partTwo s = case parseInput s of
    Success rs -> countAllowed $ combineRanges rs
    Failure x  -> error (show x)

type CountState = (Integer, Integer, [Range])

counting :: State CountState Integer
counting = do (acc, lst, rs) <- get
              case null rs of
                  True -> return $ acc + (4294967296 - (lst + 1))
                  False -> let (Range (s, e):rss) = rs
                               more = s - (lst + 1)
                           in do put (acc + more, e, rss)
                                 counting


countAllowed :: [Range] -> Integer
countAllowed rs = evalState counting (0, -1, rs)
