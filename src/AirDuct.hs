module AirDuct where

import           Data.Char   (isDigit)
import qualified Data.Heap   as H
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Vector as V
import           Debug.Trace

type Steps = Int

type Pos = (Int, Int)

data Point = Point Steps Pos (M.Map Char Pos) Pos
    deriving (Eq, Show)

instance Ord Point where
    (<=) (Point s1 p1 v1 st) (Point s2 p2 v2 _) =
--        s1 + estDist p1 target <= s2 + estDist p2 target
        -- if null v1 && null v2
        -- then estDist p1 st <= estDist p2 st
        -- else (length v1, s1) <= (length v2, s2)
        length v1 + estDist p1 st <= length v2 + estDist p2 st

data Layout = Layout (V.Vector (V.Vector Char))
    deriving Show

parseInput :: String -> Layout
parseInput s = Layout $ V.fromList $ map V.fromList (lines s)

nodes :: Layout -> M.Map Char Pos
nodes (Layout l) =
    let a = fmap (\x -> V.zip (V.fromList [0..(V.length x)]) x) l
        b = V.zip (V.fromList [0..(V.length l)]) a
        c = V.concatMap (\(i, v) -> fmap (\(j, ch) -> (ch, (i,j))) v) b
    in M.fromList $ V.toList $ V.filter (\(chr, _) -> isDigit chr) c


getSpace :: Layout -> Pos -> Char
getSpace (Layout l) (x, y) = l V.! x V.! y

estDist :: Pos -> Pos -> Int
estDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

initHeap :: M.Map Char Pos -> Pos -> H.MinHeap Point
initHeap ns p = H.singleton (Point 0 p ns p)

openSpace :: Layout -> (Int, Int) -> Bool
openSpace l (x, y) = getSpace l (x, y) /= '#'

stepsThroughNodes :: Layout -> M.Map Char Pos -> Int
stepsThroughNodes l n = stepsThroughNodesHelp l
                                              (initHeap (M.delete '0' n)
                                                        (n M.! '0'))
                                              M.empty
                                              (n M.! '0')
                                              Nothing

stepsThroughNodesHelp :: Layout
                      -> H.MinHeap Point
                      -> M.Map (Pos, M.Map Char Pos) Int
                      -> Pos
                      -> Maybe Int
                      -> Int
stepsThroughNodesHelp l h visited start best =
    case H.view h of
        Nothing -> case best of
            Just x  -> x
            Nothing -> error "no more points to explore??"
        Just (p@(Point steps pos uv _), newH) ->
            if null uv && pos == start
            then let newBest = case best of
                         Nothing -> Just steps
                         Just x  -> Just $ minimum [ x, steps ]
                     moreH = H.filter (lowercost newBest) newH
                 in
                     trace (show p) $
                         stepsThroughNodesHelp l moreH visited start newBest
            else let newPoints = H.fromList (neighbors l visited p)
                     newHeap = case best of
                         Nothing -> H.union newH newPoints
                         Just _ -> H.filter (lowercost best)
                                            (H.union newH newPoints)
                     newVisited = case M.lookup (pos, uv) visited of
                         Nothing -> M.insert (pos, uv) steps visited
                         Just s -> if s > steps
                                   then M.insert (pos, uv) steps visited
                                   else visited
                 in
                     stepsThroughNodesHelp l newHeap newVisited start best

lowercost :: Maybe Int -> Point -> Bool
lowercost (Just s1) (Point s2 _ _ _) = s1 >= s2
lowercost Nothing _                  = True

neighbors :: Layout -> M.Map (Pos, M.Map Char Pos) Int -> Point -> [Point]
neighbors l visited (Point steps (x, y) uv st) =
    let cand = [ (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1) ]
        new = filter (openSpace l) cand
        ps = map (\pos -> (pos, updateVisited pos uv)) new
        updateVisited p v = let c = getSpace l p
                            in if c == '.'
                               then v
                               else M.delete c v
        pruned = filter validMove ps
        validMove (pos, v) = case M.lookup (pos, v) visited of
            Nothing -> True
            Just s  -> s > steps + 1
    in
        map (\(p, v) -> Point (steps + 1) p v st) pruned

partOne :: String -> Int
partOne s = let l = parseInput s
                ns = nodes l
            in stepsThroughNodes l ns

-- Plan:
-- Identify all nodes (nodes)
-- Compute distances between all pairs of nodes using A*
-- Start at position '0'. Compute the permutations of the nodes
-- Compute length of path for each permutation from computed distances

-- Bad plan. All distances between pairs is too many distances

-- new plan:
-- Identify all nodes (nodes)
-- Start at position '0'. Compute distances to all other nodes
-- BFS search, visiting unvisited nodes at each step. Cache the distances using a lazy map

testInput = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
