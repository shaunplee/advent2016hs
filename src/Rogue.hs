module Rogue where

inputString :: String
inputString = ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"

input :: [Bool]
input = map (== '.') inputString

safe :: (Bool, Bool, Bool) -> Bool
safe (False, False, True) = False
safe (True, False, False) = False
safe (False, True, True)  = False
safe (True, True, False)  = False
safe _                    = True

nextRow :: [Bool] -> [Bool]
nextRow row = let l = True : row
                  c = row
                  r = tail row ++ [True]
                  lcr = zip3 l c r
              in map safe lcr

nextRowCount  :: [Bool] -> Int
nextRowCount row = let l = True : row
                       c = row
                       r = tail row ++ [True]
                       lcr = zip3 l c r
                   in length $ filter id $ map safe lcr

partOne :: Int
partOne = sum $ map (length . filter id) (take 400000 $ iterate nextRow input)
