module IP7 where

import           Control.Applicative
import           Data.List
import           Text.Trifecta

testInputYes :: String
testInputYes = "abba[bnop]qrst"

testInputNo :: String
testInputNo = "abcd[bddb]xyyx"

testInputNo2 :: String
testInputNo2 = "aaaa[qwer]tyui"

testInputYes2 :: String
testInputYes2 = "ioxxoj[asdfgh]zxcvbn"

-- newtype Hypernet = Hypernet String

-- newtype Non = Non String

data Ip = Hypernet String
        | Supernet String
        deriving Show

countValidIps :: String -> Int
countValidIps s = length $ filter validIpString $ lines s

validIpString :: String -> Bool
validIpString input = validLine $ parseLine input

validLine :: Result [Ip] -> Bool
validLine (Failure _)  = False
validLine (Success xs) =
    let (ss, hs) = splitSuperHyper xs
    in
        all validIp hs && any validIp ss

splitSuperHyper :: [Ip] -> ([Ip], [Ip])
splitSuperHyper = foldr (\x (s, h) -> case x of
                             Hypernet _ -> (s, x : h)
                             Supernet _ -> (x : s, h))
                        ([], [])

validIp :: Ip -> Bool
validIp (Hypernet xs) = not $ containsRev xs
validIp (Supernet xs) = containsRev xs

containsRev :: String -> Bool
containsRev (w : x : y : z : xs) =
    (w == z && x == y && w /= x) ||
        containsRev (x : y : z : xs)
containsRev _ = False

parseLine :: String -> Result [Ip]
parseLine = parseString ipParser mempty

ipParser :: Parser [Ip]
ipParser = some (hypernetParser <|> supernetParser)

hypernetParser :: Parser Ip
hypernetParser = do
    _ <- char '['
    x <- some letter
    _ <- char ']'
    return $ Hypernet x

supernetParser :: Parser Ip
supernetParser = do
    x <- some letter
    return $ Supernet x

-- Part 2
countSSL :: String -> Int
countSSL s = length $ filter supportsSSL $ lines s

supportsSSL :: String -> Bool
supportsSSL input = let (ss, hs) = case parseLine input of
                            Success xs -> splitSuperHyper xs
                            Failure _  -> ([], [])
                        abas = findAllAbas ss
    in any (containsBab hs) abas

findAllAbas :: [Ip] -> [String]
findAllAbas = concatMap findAbas
  where
    findAbas (Supernet (x : y : z : xs)) =
        if x == z
        then [ x, y, z ] : (findAbas $ Supernet (y : z : xs))
        else findAbas (Supernet (y : z : xs))
    findAbas _ = []

containsBab :: [Ip] -> String -> Bool
containsBab hs [ x, y, z ] =
    any (hasBab [ y, x, y ]) hs
containsBab _ _ = False

hasBab :: String -> Ip -> Bool
hasBab bab (Hypernet (x : y : z : xs)) =
    bab == [ x, y, z ] || (hasBab bab (Hypernet (y : z : xs)))
hasBab _ _ = False
