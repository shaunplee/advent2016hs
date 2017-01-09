module Grid where

import qualified Data.Heap       as H
import qualified Data.Map.Strict as M
import           Text.Trifecta

data Node = Node { pos   :: (Integer, Integer),
                   size  :: Integer,
                   used  :: Integer,
                   avail :: Integer,
                   use   :: Integer }
    deriving Show

instance Eq Node where
    (==) x y = pos x == pos y

data State = State { steps     :: Integer
                   , loc       :: (Integer, Integer)
                   , dst       :: (Integer, Integer)
                   , nodeState :: M.Map (Integer, Integer) Node
                   }
    deriving (Eq, Show)

instance Ord State where
    (<=) s1 s2 = steps s1 + estDist (loc s1) (dst s1) <=
        steps s2 + estDist (loc s2) (dst s2)

findViablePairs :: M.Map (Integer, Integer) Node -> [(Node, Node)]
findViablePairs ns = concatMap (\x -> zip (repeat x)
                                          (filter (viablePair x) (M.elems ns)))
                               ns

viablePairs :: Node -> [Node] -> [Node]
viablePairs x = filter (viablePair x)

viablePair :: Node -> Node -> Bool
viablePair x y = used x /= 0 &&
                 x /= y &&
                 used x < avail y

partOne :: String -> Int
partOne s = let nodes = parseInput s
            in
                length $ findViablePairs nodes


estDist :: (Integer, Integer) -> (Integer, Integer) -> Integer
estDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

initialState :: M.Map (Integer, Integer) Node -> H.MinHeap State
initialState ns = H.singleton (State 0 (x, 0) (0, 0) ns)
  where
    (x,_) = last $ M.keys ns

partTwo :: String -> Integer
partTwo s = let nodes = parseInput s
                start = initialState nodes
            in
                partTwoHelp start

partTwoHelp :: H.MinHeap State -> Integer
partTwoHelp moves = case H.view moves of
    Nothing -> error "no more moves????"
    Just (nextMove, restMoves) ->
        if (loc nextMove) == (0,0)
        then (steps nextMove)
        else let newPoints = undefined
             in undefined


move :: M.Map (Integer, Integer) Node
     -> Node
     -> Node
     -> (Int, M.Map (Integer, Integer) Node)
move nodes from to = if avail to >= used from
                     then if adjacent from to
                          then (1, directMove nodes from to)
                          else undefined
                     else undefined


-- Assumes the nodes are adjacent
directMove :: M.Map (Integer, Integer) Node
           -> Node
           -> Node
           -> M.Map (Integer, Integer) Node
directMove nodes from to =
    let newFrom = Node (pos from) (size from) 0 (size from) 0
        newUsed = used to + used from
        newTo = Node (pos to)
                     (size to)
                     newUsed
                     (size to - newUsed)
                     (100 * newUsed `div` (size to))
    in
        M.update (const $ Just newFrom)
                 (pos from)
                 (M.update (const $ Just newTo) (pos to) nodes)

adjacent :: Node -> Node -> Bool
adjacent na nb = let (x1, y1) = pos na
                     (x2, y2) = pos nb
                     a = abs (x1 - x2)
                     b = abs (y1 - y2)
                 in
                     case a of
                         0 -> b == 1
                         1 -> b == 0
                         _ -> False

-- Input parsing

parseInput :: String -> M.Map (Integer, Integer) Node
parseInput s = case traverse (parseString parseNode mempty) (lines s) of
    Success x -> M.fromList [(pos n, n) | n <- x]
    Failure x -> error (show x)

parseNode :: Parser Node
parseNode = do
    _ <- string "/dev/grid/node-x"
    x <- decimal
    _ <- string "-y"
    y <- decimal
    _ <- spaces
    s <- parseSize
    _ <- spaces
    u <- parseSize
    _ <- spaces
    a <- parseSize
    _ <- spaces
    us <- decimal
    _ <- char '%'
    return $ Node (x, y) s u a us

parseSize :: Parser Integer
parseSize = do
    x <- decimal
    _ <- char 'T'
    return x

testInputTwo = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"

testInput :: String
testInput = "/dev/grid/node-x0-y0     90T   69T    21T   76%\n/dev/grid/node-x0-y1     88T   71T    17T   80%\n/dev/grid/node-x0-y2     92T   64T    28T   69%\n/dev/grid/node-x0-y3     89T   66T    23T   74%"
