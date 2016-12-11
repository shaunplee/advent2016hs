module Compression where

import           Control.Applicative
import           Text.Trifecta

newtype Marker = Marker (Int, Int)
    deriving Show

data Remaining = Compressed Marker String
               | Clear String String
    deriving Show

decompress :: String -> String
decompress [] = ""
decompress input = case step input of
    Compressed (Marker (c, t)) rest ->
        let rep = take c rest
            newRest = drop c rest
        in concat (replicate t rep) ++ decompress newRest
    Clear i rest -> i ++ decompress rest

decompressedLength :: String -> Integer
decompressedLength [] = 0
decompressedLength input =
    case step input of
        Compressed (Marker (c, t)) rest ->
            toInteger t * decompressedLength (take c rest) +
                decompressedLength (drop c rest)
        Clear i rest -> toInteger (length i) + decompressedLength rest

step :: String -> Remaining
step input = case parseString (stepM <|> stepL) mempty input of
    Success x -> x
--    Failure x -> x -- bad bad bad

stepL :: Parser Remaining
stepL = do
    i <- some (noneOf "(")
    r <- many (noneOf "\n")
    return $ Clear i r

stepM :: Parser Remaining
stepM = do
    m <- marker
    r <- many (noneOf "\n")
    return $ Compressed m r

marker :: Parser Marker
marker = do
    _ <- char '('
    x <- decimal
    _ <- char 'x'
    y <- decimal
    _ <- char ')'
    return $ Marker (fromInteger x, fromInteger y)
