module Microchips where

import qualified Data.Foldable as F
import qualified Data.List     as L
import           Data.Maybe    (catMaybes)
import qualified Data.Set      as S
import qualified Data.Vector   as V
--import           Debug.Trace   (trace)

data Object = Gen Element
            | Chip Element
    deriving (Eq, Ord, Show)

data Element = Th | Pl | St | Pr | Ru | El | Di
    deriving (Eq, Ord, Show)

-- instance Show Element where
--     show x = case x of
--         Th -> "Th"
--         Pl -> "Pl"
--         St -> "St"
--         Pr -> "Pr"
--         Ru -> "Ru"

newtype Status = Status (V.Vector (S.Set Object))
    deriving (Ord, Show)

instance Eq Status where
    (==) x y =
        let elements = [ Th, Pl]--, St, Pr, Ru, El, Di ]
            pairsX = L.sort (map (findPair x) elements)
            pairsY = L.sort (map (findPair y) elements)
        in
            pairsX == pairsY

findPair :: Status -> Element -> (Int, Int)
findPair (Status s) e = let g = V.head $ V.filter snd $
                                V.zip (V.enumFromN 0 4)
                                      (V.map (S.member (Gen e)) s)
                            c = V.head $ V.filter snd $
                                V.zip (V.enumFromN 0 4)
                                      (V.map (S.member (Chip e)) s)
                        in
                            (fst g, fst c)

newtype Floor = Floor Int
    deriving (Eq, Ord, Show)

newtype State = State (Floor, Status)
    deriving (Eq, Ord, Show)

showState :: State -> String
showState (State (Floor f, Status s)) =
    let showFloor (i, obs) = "F" ++ (show i) ++ ": " ++ show obs ++ "\n"
        floors = concatMap showFloor (zip [ 4, 3, 2, 1 ] (reverse (V.toList s)))
    in
        "Floor" ++ (show $ f + 1) ++ "\n" ++ floors

data Move = Move Int (S.Set Object)
    deriving (Eq, Ord, Show)

testState :: State
testState = State (Floor 0
                  , Status $ V.fromList [ S.fromList [Chip Th,
                                                      Chip Pl]
                                        , S.fromList [Gen Th]
                                        , S.fromList [Gen Pl]
                                        , S.empty])

inputState :: State
inputState = State
    ( Floor 0
    , Status $
        V.fromList [ S.fromList [ Gen Th
                                , Chip Th
                                , Gen Pl
                                , Gen St
                                , Gen El
                                , Gen Di
                                , Chip El
                                , Chip Di
                                ]
                   , S.fromList [ Chip Pl, Chip St ]
                   , S.fromList [ Gen Pr
                                , Chip Pr
                                , Gen Ru
                                , Chip Ru
                                ]
                   , S.empty
                   ]
    )

partOneRun = partOne inputState

partOne :: State -> Int
partOne s = partOneHelp 0 S.empty [ s ]
  where
    partOneHelp cnt visited ss
        | L.any win ss = cnt
        | otherwise = let newVisited = S.union visited (S.fromList ss)
                      in
                          partOneHelp (cnt + 1)
                                      newVisited
                                      (filter (`S.notMember` newVisited)
                                              (newStates ss))
--                                  (newStates ss)



newStates :: [State] -> [State]
newStates s = L.nub $ L.concatMap exploreMoves s

exploreMoves :: State -> [State]
exploreMoves s = let moves = generateMoves s
                     ns = fmap (applyMove s) (S.toAscList moves)
                 in catMaybes ns

win :: State -> Bool
win (State (_, Status s)) = S.size (s V.! 3) == 10

okState :: State -> Bool
okState (State (_, Status s)) = V.all okFloor s

okFloor :: S.Set Object -> Bool
okFloor os = let gs = S.filter (\x -> case x of Gen _ -> True; _ -> False) os
                 cs = S.filter (\x -> case x of Chip _ -> True; _ -> False) os
             in
                 if S.size gs == 0 then True
                 else
                     all (\x -> F.any (matchObjects x) gs) cs

matchObjects :: Object -> Object -> Bool
matchObjects (Gen gen) (Chip chip) = gen == chip
matchObjects (Chip chip) (Gen gen) = gen == chip
matchObjects (Gen gen) (Gen g2)    = gen == g2
matchObjects (Chip c1) (Chip c2)   = c1 == c2

generateMoves :: State -> S.Set Move
generateMoves (State ((Floor from), (Status s))) =
    let objects = combinations (s V.! from)
--        singleObjects = S.map S.singleton (s V.! from)
    in
        case from of
            0 -> S.map (Move 1) objects
            1 -> S.map (Move 0) objects `S.union`
                S.map (Move 2) objects
            2 -> S.map (Move 1) objects `S.union`
                S.map (Move 3) objects
            3 -> S.map (Move 2) objects
            _ -> S.empty

combinations :: S.Set Object -> S.Set (S.Set Object)
combinations os = case S.size os of
    0 -> S.empty
    1 -> S.singleton os
    _ -> let los = S.toAscList os
             car = head los
             cdr = L.drop 1 los
         in
             S.singleton car `S.insert`
                 S.fromList (fmap (\y -> S.fromList [ car, y ]) cdr) `S.union`
                 combinations (S.fromList cdr)

applyMove :: State -> Move -> Maybe State
applyMove (State ((Floor from), (Status s))) (Move to os) =
    let oldFrom = S.toList $ s V.! from
        newFrom = S.fromList $ Prelude.foldr L.delete oldFrom os
        oldTo = s V.! to
        newTo = oldTo `S.union` os
        newState = State ( Floor to
                         , Status $ s V.// [ (from, newFrom), (to, newTo) ]
                         )
    in
        if okState newState then Just newState else Nothing
