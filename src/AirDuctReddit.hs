{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AirDuctReddit where

import           BasePrelude  hiding ((&))
import           Control.Lens
import qualified Data.Map     as Map
import qualified Data.Set     as Set

type Pos = (Int, Int)

graph :: String -> Map.Map Pos Char
graph input = Map.fromList [ ((x, y), c)
                           | (y, s) <- zip [0 ..] (lines input)
                           , (x, c) <- zip [0 ..] s ]

wire :: Char -> Map.Map Pos Char -> (Char, Int, Int)
wire needle g = head [ (c, x, y)
                     | ((x, y), c) <- Map.toList g
                     , c == needle ]

--neighbors :: Map.Map Pos Char -> (Char, Int, Int) -> [(Char, Int, Int)]
neighbors g (_, x, y) = concatMap try
                                  [ (x - 1, y)
                                  , (x + 1, y)
                                  , (x, y - 1)
                                  , (x, y + 1)
                                  ]
    where try (x, y) = [(c, x, y) | Just c <- [g ^. at (x, y)], c /= '#']

dist :: (Char, Int, Int)
     -> (Char, Int, Int)
     -> ((Char, Int, Int) -> [(Char, Int, Int)])
     -> Int
dist i goal ns = rec (Set.singleton i) 0
  where
    rec frontier steps
        | Set.null frontier = error "invalid goal story"
        | goal `elem` Set.toList frontier =
              steps
        | otherwise = rec (Set.fromList . concatMap ns . Set.toList $ frontier)
                           (steps + 1)

--measure :: Map.Map (Char, Char) Int -> [Char] -> (Char, [(x, y)])
--measure :: Char -> [Char] -> (Char, [(Char, Char)])
measure dists = foldl' (\(lst, acc) next -> (next, (fnd lst next) : acc))
                       ('0', [])
    where fnd x y = fromJust $ dists ^. at (x, y) <|> dists ^. at (y, x)

comb :: Int -> String -> [String]
comb 0 _        = [[]]
comb _ []       = []
comb m (x : xs) = map (x :) (comb (m - 1) xs) ++ comb m xs

partOne :: String -> String -> Int
partOne input nodes = minimum $ map (sum . snd . measure dists) (permutations numbers)
  where
    (numbers, g) = (nodes, graph input)
    dists = Map.fromList [ ( (src, dst)
                           , dist (wire src g) (wire dst g) (neighbors g)
                           )
                         | [ src, dst ] <- comb 2 ('0' : numbers) ]

partTwo :: String -> String -> Int
partTwo input nodes = minimum $ map (sum . snd . measure dists . (<> "0")) (permutations numbers)
  where
    (numbers, g) = (nodes, graph input)
    dists = Map.fromList [ ( (src, dst)
                           , dist (wire src g) (wire dst g) (neighbors g)
                           )
                         | [ src, dst ] <- comb 2 ('0' : numbers) ]

testInput = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
