module Scrambled where

import           Control.Applicative
import           Data.Foldable       (foldl')
import           Data.List           (permutations)
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import           Text.Trifecta

--data Rotate = undefined

type Letter = Char
type Pos = Int
type Steps = Int

data Op = SwapP Pos Pos
        | SwapL Letter Letter
        | RotateS Steps
        | RotateL Letter
        | Reverse Pos Pos
        | Move Pos Pos
    deriving Show

swapP :: Parser Op
swapP = do
    _ <- string "swap position "
    x <- decimal
    _ <- string " with position "
    y <- decimal
    return $ SwapP (fromInteger x) (fromInteger y)

swapL :: Parser Op
swapL = do
    _ <- string "swap letter "
    x <- letter
    _ <- string " with letter "
    y <- letter
    return $ SwapL x y

rotateS :: Parser Op
rotateS = do
    _ <- string "rotate "
    dir <- (string "right ") <|> (string "left ")
    let d = case dir of
            "right " -> 1
            "left "  -> -1
    s <- decimal
    _ <- if s == 1 then string " step" else string " steps"
    return $ RotateS $ fromInteger $ d * s

rotateL :: Parser Op
rotateL = do
    _ <- string "rotate based on position of letter "
    x <- letter
    return $ RotateL x

reversePP :: Parser Op
reversePP = do
    _ <- string "reverse positions "
    x <- decimal
    _ <- string " through "
    y <- decimal
    return $ Reverse (fromInteger x) (fromInteger y)

move :: Parser Op
move = do
    _ <- string "move position "
    x <- decimal
    _ <- string " to position "
    y <- decimal
    return $ Move (fromInteger x) (fromInteger y)

op :: Parser Op
op = swapP <|> swapL <|> rotateL <|> rotateS <|> reversePP <|> move

parseInput :: String -> Result [Op]
parseInput = traverse (parseString op mempty) . lines

testInput :: String
testInput = "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d\n"

scramble :: [Op] -> String -> String
scramble operations s = V.toList (foldl' executeOp (V.fromList s) operations)

loadInput :: String -> [Op]
loadInput s = case parseInput s of
                  Success x -> x
                  Failure x -> error (show x)

runScramble :: String -> String -> String
runScramble ops = scramble (loadInput ops)

executeOp :: V.Vector Char -> Op -> V.Vector Char
executeOp s (SwapP x y)   = s V.// [(x, s V.! y), ( y, s V.! x)]
executeOp s (SwapL x y)   = let swapChar c
                                    | c == x = y
                                    | c == y = x
                                    | otherwise = c
                            in
                                V.map swapChar s
executeOp s (RotateS x)
    | x < 0 = let y = (-x) in
                  V.drop y s V.++ V.take y s
    | x > 0 = let l = V.length s - x
              in
                  V.drop l s V.++ V.take l s
    | otherwise = s
executeOp s (RotateL x)   = let i = fromMaybe 0 (V.findIndex (== x) s)
                                oneStep = executeOp s (RotateS 1)
                                iStep = executeOp oneStep (RotateS i)
    in
                                if i >= 4 then
                                    executeOp iStep (RotateS 1)
                                else
                                    iStep
executeOp s (Reverse x y) = let front = V.take x s
                                back = V.drop (y + 1) s
                                middle = V.slice x (y - x + 1) s
                            in
                                front V.++ V.reverse middle V.++ back
executeOp s (Move x y)    = let (front, back) = V.splitAt x s
                                new = front V.++ V.tail back
                                (newFront, newBack) = V.splitAt y new
                            in
                                newFront V.++ V.take 1 back V.++ newBack

partTwo :: String -> String -> String
partTwo ops s = let scr = scramble (loadInput ops)
    in
    head $ filter (\a -> scr a == s) (permutations "abcdefgh")
