module Triangles where

import           Data.List     (sort)
import           Text.Trifecta

newtype Triangle = Triangle (Integer, Integer, Integer)
    deriving Show

countValidTriangles :: String -> Int
countValidTriangles input = length $ filter validTriangle $ parseInput input

validTriangle :: Triangle -> Bool
validTriangle (Triangle (a, b, c)) =
    let [x, y, z] = sort [a, b, c]
    in x + y > z

parseInput :: String -> [Triangle]
parseInput input = case traverse parseLine $ lines input of
    Success x -> x
    Failure _ -> []

parseLine :: String -> Result Triangle
parseLine = parseString parseTriangle mempty

parseTriangle :: Parser Triangle
parseTriangle = do
    _ <- spaces
    a <- decimal
    _ <- spaces
    b <- decimal
    _ <- spaces
    c <- decimal
    return $ Triangle (a, b, c)


-- Part 2
countValidTrianglesCols :: String -> Int
countValidTrianglesCols input =
    length $ filter validTriangle $ parseInputCol input

parseInputCol :: String -> [Triangle]
parseInputCol input = case parseString (some parseThreeLines) mempty input of
    Success x -> concat x
    Failure _ -> []

parseThreeLines :: Parser [Triangle]
parseThreeLines = do
    Triangle (a1, a2, a3) <- parseTriangle
    _ <- newline
    Triangle (b1, b2, b3) <- parseTriangle
    _ <- newline
    Triangle (c1, c2, c3) <- parseTriangle
    _ <- newline
    return [Triangle (a1, b1, c1),
            Triangle (a2, b2, c2),
            Triangle (a3, b3, c3)]
