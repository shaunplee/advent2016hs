module Signal where

import           Data.List (sortOn)
import           Data.Map

testInput :: String
testInput = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"

decodeMessage :: String -> String
decodeMessage input = Prelude.map findMaxVal $ Prelude.foldr countChars start ms
    where ms = lines input
          start = replicate (length (head ms)) empty

findMaxVal :: Map Char Integer -> Char
findMaxVal m =  fst $ head $ sortOn (\(x, y) -> (y, x)) (toList m)

countChars :: String -> [Map Char Integer] -> [Map Char Integer]
countChars =  zipWith insertChar

insertChar :: Char -> Map Char Integer -> Map Char Integer
insertChar c = insertWith (+) c 1
