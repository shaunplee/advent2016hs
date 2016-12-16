module Sculpture where

data Disc = Disc { positions :: Int
                 , start     :: Int
                 }

input :: [Disc]
input = [ Disc 13 10, Disc 17 15, Disc 19 17, Disc 7 1, Disc 5 0, Disc 3 1 ]

inputTwo :: [Disc]
inputTwo = [ Disc 13 10
           , Disc 17 15
           , Disc 19 17
           , Disc 7 1
           , Disc 5 0
           , Disc 3 1
           , Disc 11 0
           ]

testInput :: [Disc]
testInput = [ Disc 5 4, Disc 2 1]

partOne :: [Disc] -> Int
partOne ds = let tds = zip [1..] ds
             in
                 head $ filter (aligned tds) [0..]

aligned :: [(Int, Disc)] -> Int -> Bool
aligned tds t = all (== 0) $
                    map
                    (\(s, d) -> (t + s + start d) `mod` positions d)
                    tds
