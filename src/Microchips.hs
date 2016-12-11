module Microchips where

import           Data.List   as L
import           Data.Maybe  (catMaybes)
import           Data.Vector as V

data Object = Gen Element
            | Chip Element
    deriving (Eq, Show)

data Element = Thulium | Plutonium | Strontium | Promethium | Ruthenium
    deriving (Eq)

instance Show Element where
    show x = case x of
        Thulium    -> "Th"
        Plutonium  -> "Pl"
        Strontium  -> "St"
        Promethium -> "Pr"
        Ruthenium  -> "Ru"

newtype Status = Status (Vector (Vector Object))
    deriving (Eq, Show)

newtype Floor = Floor Int
    deriving (Eq, Show)

newtype State = State (Floor, Status)
    deriving (Eq, Show)

data Move = Move Int (V.Vector Object)
    deriving Show

testState :: State
testState = State (Floor 0
                  , Status $ V.fromList [ V.fromList [Chip Thulium,
                                                      Chip Plutonium]
                                        , V.fromList [Gen Thulium]
                                        , V.fromList [Gen Plutonium]
                                        , V.empty])


inputState :: State
inputState = State
    ( Floor 0
    , Status $
        V.fromList [ V.fromList [ Gen Thulium
                                , Chip Thulium
                                , Gen Plutonium
                                , Gen Strontium
                                ]
                   , V.fromList [ Chip Plutonium, Chip Strontium ]
                   , V.fromList [ Gen Promethium
                                , Chip Promethium
                                , Gen Ruthenium
                                , Chip Ruthenium
                                ]
                   , V.empty
                   ]
    )

partOneRun = partOne inputState

partOne :: State -> Int
partOne s = partOneHelp 0 [ s ]
  where
    partOneHelp cnt ss = if L.any win ss
                         then cnt
                         else if cnt > 11 then -1
                              else
                             partOneHelp (cnt + 1)
                                          (L.nub (L.concatMap exploreMoves ss))


exploreMoves :: State -> [State]
exploreMoves s = let moves = generateMoves s
                     ns = fmap (applyMove s) moves
                 in catMaybes ns

win :: State -> Bool
win (State (_, (Status s))) = V.length (s V.! 3) == 4

okState :: State -> Bool
okState (State (_, Status s)) = V.all okFloor s

okFloor :: Vector Object -> Bool
okFloor os = let gs = V.filter (\x -> case x of Gen _ -> True; _ -> False) os
                 cs = V.filter (\x -> case x of Chip _ -> True; _ -> False) os
             in
                 if V.length gs == 0 then True
                 else
                     V.all (\x -> V.any (matchObjects x) gs) cs

matchObjects :: Object -> Object -> Bool
matchObjects (Gen gen) (Chip chip) = gen == chip
matchObjects (Chip chip) (Gen gen) = gen == chip
matchObjects (Gen gen) (Gen g2)    = gen == g2
matchObjects (Chip c1) (Chip c2)   = c1 == c2

generateMoves :: State -> [Move]
generateMoves (State ((Floor from), (Status s))) =
    let objects = toList (combinations (s V.! from))
    in
        case from of
            0 -> fmap (Move 1) objects
            1 -> fmap (Move 0) objects Prelude.++
                fmap (Move 2) objects
            2 -> fmap (Move 1) objects Prelude.++
                fmap (Move 3) objects
            3 -> fmap (Move 2) objects
            _ -> []

combinations :: Vector Object -> Vector (Vector Object)
combinations os = case V.length os of
    0 -> V.empty
    1 -> V.empty
    _ -> let car = V.head os
             cdr = V.drop 1 os
         in
             V.singleton car `V.cons` V.map (\y -> fromList [ car, y ]) cdr V.++
                  combinations cdr

applyMove :: State -> Move -> Maybe State
applyMove (State ((Floor from), (Status s))) (Move to os) =
    let oldFrom = toList $ s V.! from
        newFrom = V.fromList $ Prelude.foldr L.delete oldFrom os
        oldTo = s V.! to
        newTo = oldTo V.++ os
        newState = State ( Floor to
                         , Status $ s // [ (from, newFrom), (to, newTo) ]
                         )
    in
        if okState newState then Just newState else Nothing
