module AirDuct where

import           Data.Char     (isDigit)
import qualified Data.Heap     as H
import           Data.List     (permutations, sortBy)
import qualified Data.Map      as M
import qualified Data.Map.Lazy as LM
import qualified Data.Vector   as V
import           Debug.Trace

type Steps = Int

type Pos = (Int, Int)

data Point = Point Steps Pos (M.Map Char Pos)
    deriving (Eq, Show)

instance Ord Point where
    (<=) (Point s1 p1 v1) (Point s2 p2 v2) =
--        s1 + estDist p1 target <= s2 + estDist p2 target
        (length v1, s1) <= (length v2, s2)

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

estDist :: (Int, Int) -> (Int, Int) -> Int
estDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

initHeap :: M.Map Char Pos -> Pos -> H.MinHeap Point
initHeap ns p = H.singleton (Point 0 p ns)

openSpace :: Layout -> (Int, Int) -> Bool
openSpace l (x, y) = getSpace l (x, y) /= '#'

stepsThroughNodes :: Layout -> M.Map Char Pos -> Int
stepsThroughNodes l n = stepsThroughNodesHelp l
                                              (initHeap (M.delete '0' n)
                                                        (n M.! '0'))
                                              M.empty
                                              Nothing

stepsThroughNodesHelp :: Layout
                      -> H.MinHeap Point
                      -> M.Map (Pos, M.Map Char Pos) Int
                      -> Maybe Int
                      -> Int
stepsThroughNodesHelp l h visited best =
    case H.view h of
        Nothing -> case best of
            Just x  -> x
            Nothing -> error "no more points to explore??"
        Just (p@(Point steps pos uv), newH) ->
            if null uv
            then
            -- compare with best, filter out candidates with cost
            -- > best, recursively continue
                let newBest = case best of
                        Nothing -> Just steps
                        Just x  -> Just $ minimum [ x, steps ]
                    moreH = H.filter (lowercost newBest) newH
                in
                    trace (show p) $ stepsThroughNodesHelp l moreH visited newBest
            else let newPoints = H.fromDescList (neighbors l visited p)
                     newVisited = case M.lookup (pos, uv) visited of
                         Nothing -> M.insert (pos, uv) steps visited
                         Just s -> if s > steps
                                   then M.insert (pos, uv) steps visited
                                   else visited
                     newHeap = H.union newH newPoints
                     moreH = case best of
                         Nothing -> newHeap
                         Just _  -> H.filter (lowercost best) newHeap
                 in
                     stepsThroughNodesHelp l moreH newVisited best

lowercost :: Maybe Int -> Point -> Bool
lowercost (Just s1) (Point s2 _ _) = s1 >= s2
lowercost Nothing _                = True

neighbors :: Layout -> M.Map (Pos, M.Map Char Pos) Int -> Point -> [Point]
neighbors l visited (Point steps (x, y) uv) =
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
        sortBy (flip compare) $ map (\(p, v) -> Point (steps + 1) p v) pruned

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

badState :: H.MinHeap Point
badState = H.fromList [ Point 5
                              (1, 2)
                              (M.fromList [ ('1', (1, 3))
                                          , ('2', (1, 9))
                                          , ('3', (3, 9))
                                          ])
                      , Point 6
                              (3, 5)
                              (M.fromList [ ('1', (1, 3))
                                          , ('2', (1, 9))
                                          , ('3', (3, 9))
                                          ])
                      ]

badVisited :: M.Map (Pos, M.Map Char Pos) Int
badVisited = M.fromList [(((1,1),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9))]),4),(((1,1),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9)),('4',(3,1))]),0),(((1,1),M.fromList [('2',(1,9)),('3',(3,9))]),8),(((1,1),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),4),(((1,1),M.fromList [('3',(3,9)),('4',(3,1))]),16),(((1,2),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9)),('4',(3,1))]),1),(((1,2),M.fromList [('2',(1,9)),('3',(3,9))]),9),(((1,2),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),3),(((1,2),M.fromList [('3',(3,9)),('4',(3,1))]),15),(((1,3),M.fromList [('2',(1,9)),('3',(3,9))]),10),(((1,3),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),2),(((1,3),M.fromList [('3',(3,9)),('4',(3,1))]),14),(((1,4),M.fromList [('2',(1,9)),('3',(3,9))]),11),(((1,4),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),3),(((1,4),M.fromList [('3',(3,9)),('4',(3,1))]),13),(((1,5),M.fromList [('2',(1,9)),('3',(3,9))]),12),(((1,5),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),4),(((1,5),M.fromList [('3',(3,9)),('4',(3,1))]),12),(((1,5),M.fromList [('4',(3,1))]),16),(((1,6),M.fromList [('2',(1,9)),('3',(3,9))]),13),(((1,6),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),5),(((1,6),M.fromList [('3',(3,9)),('4',(3,1))]),11),(((1,6),M.fromList [('4',(3,1))]),15),(((1,7),M.fromList [('2',(1,9)),('3',(3,9))]),14),(((1,7),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),6),(((1,7),M.fromList [('3',(3,9)),('4',(3,1))]),10),(((1,7),M.fromList [('4',(3,1))]),14),(((1,8),M.fromList [('2',(1,9)),('3',(3,9))]),15),(((1,8),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),7),(((1,8),M.fromList [('3',(3,9)),('4',(3,1))]),9),(((1,8),M.fromList [('4',(3,1))]),13),(((1,9),M.fromList [('3',(3,9))]),16),(((1,9),M.fromList [('3',(3,9)),('4',(3,1))]),8),(((1,9),M.fromList [('4',(3,1))]),12),(((2,1),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9))]),3),(((2,1),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9)),('4',(3,1))]),1),(((2,1),M.fromList [('2',(1,9)),('3',(3,9))]),7),(((2,1),M.fromList [('2',(1,9)),('3',(3,9)),('4',(3,1))]),5),(((2,9),M.fromList [('2',(1,9))]),15),(((2,9),M.fromList [('3',(3,9)),('4',(3,1))]),9),(((2,9),M.fromList [('4',(3,1))]),11),(((3,1),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9))]),2),(((3,1),M.fromList [('2',(1,9)),('3',(3,9))]),6),(((3,2),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9))]),3),(((3,2),M.fromList [('2',(1,9)),('3',(3,9))]),7),(((3,3),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9))]),4),(((3,3),M.fromList [('2',(1,9)),('3',(3,9))]),8),(((3,3),M.fromList [('4',(3,1))]),16),(((3,4),M.fromList [('1',(1,3)),('2',(1,9)),('3',(3,9))]),5),(((3,4),M.fromList [('2',(1,9)),('3',(3,9))]),9),(((3,4),M.fromList [('4',(3,1))]),15),(((3,5),M.fromList [('2',(1,9)),('3',(3,9))]),10),(((3,5),M.fromList [('4',(3,1))]),14),(((3,6),M.fromList [('2',(1,9)),('3',(3,9))]),11),(((3,6),M.fromList [('4',(3,1))]),13),(((3,7),M.fromList [('2',(1,9))]),16),(((3,7),M.fromList [('2',(1,9)),('3',(3,9))]),12),(((3,7),M.fromList [('4',(3,1))]),12),(((3,8),M.fromList [('2',(1,9))]),15),(((3,8),M.fromList [('2',(1,9)),('3',(3,9))]),13),(((3,8),M.fromList [('4',(3,1))]),11),(((3,9),M.fromList [('2',(1,9))]),14),(((3,9),M.fromList [('4',(3,1))]),10)]
