module Cubicles where

import           Data.Char (intToDigit)
import qualified Data.Heap as H
import           Data.List (sortBy)
import qualified Data.Map  as M
import           Numeric   (showIntAtBase)

data Point = Point Int (Int, Int)
    deriving (Eq, Show)

instance Ord Point where
    (<=) (Point s1 p1) (Point s2 p2) =
--        s1 + estDist p1 target <= s2 + estDist p2 target
        s1 <= s2

estDist :: (Int, Int) -> (Int, Int) -> Int
estDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

target = realTarget
favNum = realFavNum

--target = testTarget
--favNum = testFavNum

testTarget = (7, 4)
testFavNum = 10

realTarget :: (Int, Int)
realTarget = (31, 39)

realFavNum :: Int
realFavNum = 1362

initialState :: H.MinHeap Point
initialState = H.singleton (Point 0 (1,1))

countOnes :: Int -> Int
countOnes i = length $ filter (== '1') $ showIntAtBase 2 intToDigit i ""

openSpace :: (Int, Int) -> Bool
openSpace (x, y) = let s = x*x + 3*x + 2*x*y + y + y*y + favNum
                   in even $ countOnes s

reachablePositions :: Int
reachablePositions = reachablePositionsHelp initialState M.empty

reachablePositionsHelp :: H.MinHeap Point -> M.Map (Int, Int) Int -> Int
reachablePositionsHelp h visited =
    case H.view h of
        Nothing -> error "no more points to explore??"
        Just (p@(Point steps (x, y)), newH) ->
            if steps > 50
            then M.size visited
            else let newPoints = H.fromDescList (neighbors visited p)
                     newVisited = case M.lookup (x, y) visited of
                         Nothing -> M.insert (x, y) steps visited
                         Just s -> if s > steps
                                   then M.insert (x, y) steps visited
                                   else visited
                 in
                     reachablePositionsHelp (H.union newH newPoints) newVisited


stepsToTarget ::  Int
stepsToTarget = stepsToTargetHelp initialState M.empty

stepsToTargetHelp :: H.MinHeap Point -> M.Map (Int, Int) Int -> Int
stepsToTargetHelp h visited =
    case H.view h of
        Nothing -> error "No more points to explore??"
        Just (p@(Point steps (x, y)), newH) ->
            if (x, y) == target
            then steps
            else let newPoints = H.fromDescList (neighbors visited p)
                     newVisited = case M.lookup (x, y) visited of
                         Nothing -> M.insert (x, y) steps visited
                         Just s -> if s > steps
                                   then M.insert (x, y) steps visited
                                   else visited
                 in
                     stepsToTargetHelp (H.union newH newPoints) newVisited

neighbors :: M.Map (Int, Int) Int -> Point -> [Point]
neighbors visited (Point steps (x, y)) =
    let cand = [ (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1) ]
        pos = filter (\(a,b) -> a >=0 && b >=0) cand
        new = filter openSpace pos
        pruned = filter (`M.notMember` visited) new
    in
        sortBy (flip compare) $ map (Point (steps + 1)) pruned
