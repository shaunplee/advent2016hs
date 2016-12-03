module Bathroom where

import           Data.Maybe (fromJust)

bathroomCode :: String -> String
bathroomCode input = drop 1 $ map posToChar $ scanl instToPos (1,1) $ lines input

posToChar :: (Int, Int) -> Char
posToChar (0, 0) = '1'
posToChar (0, 1) = '2'
posToChar (0, 2) = '3'
posToChar (1, 0) = '4'
posToChar (1, 1) = '5'
posToChar (1, 2) = '6'
posToChar (2, 0) = '7'
posToChar (2, 1) = '8'
posToChar (2, 2) = '9'
posToChar _      = '0'

instToPos :: (Int, Int) -> String -> (Int, Int)
instToPos = foldl move

move :: (Int, Int) -> Char -> (Int, Int)
move (0, x) 'U' = (0, x)
move (y, x) 'U' = (y - 1, x)
move (2, x) 'D' = (2, x)
move (y, x) 'D' = (y + 1, x)
move (y, 0) 'L' = (y, 0)
move (y, x) 'L' = (y, x - 1)
move (y, 2) 'R' = (y, 2)
move (y, x) 'R' = (y, x + 1)


-- Part 2
dBathroomCode :: String -> String
dBathroomCode input = drop 1 $
    fromJust $
        traverse id $ map dPosToChar $ scanl dInstToPos (2, 0) $ lines input

dInstToPos :: (Int, Int) -> String -> (Int, Int)
dInstToPos = foldl dMove

dPosToChar :: (Int, Int) -> Maybe Char
dPosToChar (0, 2) = Just '1'
dPosToChar (1, 1) = Just '2'
dPosToChar (1, 2) = Just '3'
dPosToChar (1, 3) = Just '4'
dPosToChar (2, 0) = Just '5'
dPosToChar (2, 1) = Just '6'
dPosToChar (2, 2) = Just '7'
dPosToChar (2, 3) = Just '8'
dPosToChar (2, 4) = Just '9'
dPosToChar (3, 1) = Just 'A'
dPosToChar (3, 2) = Just 'B'
dPosToChar (3, 3) = Just 'C'
dPosToChar (4, 2) = Just 'D'
dPosToChar _      = Nothing

dMove :: (Int, Int) -> Char -> (Int, Int)
dMove (y, x) dir = let (dy, dx) = case dir of
                           'R' -> (0, 1)
                           'L' -> (0, -1)
                           'U' -> (-1, 0)
                           'D' -> (1, 0)
                           _   -> (0, 0)
                       ny = y + dy
                       nx = x + dx
                   in
                       case dPosToChar (ny, nx) of
                           Just c  -> (ny, nx)
                           Nothing -> (y, x)
