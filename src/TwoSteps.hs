module TwoSteps where

import           Crypto.Hash
import qualified Data.ByteString.Lazy      as LB
import           Data.ByteString.Lazy.UTF8 (fromString)
import           Data.List                 as L (partition)
import           Data.Maybe                (mapMaybe)
import           Data.Set                  as S
import           Debug.Trace               (trace)

input :: String
input = "lpvhkcbi"
--input = "kglvqrro"
--input = "hijkl"
--input = "ihgpwlah"

data Dir = Up | Dn | Lt | Rt
    deriving Eq

open :: S.Set Char
open = S.fromList "bcdef"

instance Show Dir where
    show Up = "U"
    show Dn = "D"
    show Lt = "L"
    show Rt = "R"

newtype State = State ([Dir], (Int, Int))
    deriving Show

initState :: [State]
initState = [State $ ([], (0,0))]

md5 :: String -> String
md5 s = show (hashlazy (fromString s) :: Digest MD5)

isOpen:: (Dir, Char) -> Maybe Dir
isOpen (d, c) = if S.member c open then Just d else Nothing

directions :: State -> [Dir]
directions (State (path, (x, y))) =
    let st = input ++ Prelude.concatMap show path
        m = zip [ Up, Dn, Lt, Rt ] (take 4 (md5 st))
    in
        removeBlocked (x, y) (mapMaybe isOpen m)
  where
    removeBlocked (px, py) ds =
        let ds' = if px == 0 then Prelude.filter (/= Up) ds else ds
            ds'' = if px == 3 then Prelude.filter (/= Dn) ds' else ds'
            ds''' = if py == 0 then Prelude.filter (/= Lt) ds'' else ds''
        in
            if py == 3 then Prelude.filter (/= Rt) ds''' else ds'''

nextStates :: State -> [State]
nextStates s = let dirs = directions s
               in Prelude.map (executeState s) dirs

executeState :: State -> Dir -> State
executeState (State (path, (x, y))) Up =
    State (path ++ [ Up ], (x - 1, y))
executeState (State (path, (x, y))) Dn =
    State (path ++ [ Dn ], (x + 1, y))
executeState (State (path, (x, y))) Lt =
    State (path ++ [ Lt ], (x, y - 1))
executeState (State (path, (x, y))) Rt =
    State (path ++ [ Rt ], (x, y + 1))

oneStep :: [State] -> [State]
oneStep = Prelude.concatMap nextStates

partOne :: [State] -> String
partOne [] = error "No path to vault"
partOne ss = let sol = Prelude.filter (\(State (p, pos)) -> pos == (3, 3)) ss
             in
                 if not (Prelude.null sol)
                 then case head sol of
                     State (path, _) -> Prelude.concatMap show path
                 else partOne (oneStep ss)

partTwo :: Int -> [State] -> Int
partTwo steps [] = steps
partTwo steps ss = let (sol, rest) = L.partition (\(State (_, pos)) -> pos ==
                                                      (3, 3))
                                                 ss
                   in
                       if not (Prelude.null sol)
                       then case head sol of
                           State (path, _) -> partTwo (length path)
                                                             (oneStep rest)
                       else partTwo steps (oneStep rest)
