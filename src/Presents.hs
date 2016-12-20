module Presents where

import           Control.Monad.State
import           Data.Sequence       as S
import           Data.Vector.Unboxed as V
import           Debug.Trace         (trace)


type Elf = (Int, Int)

type ElfState = (Int, V.Vector Elf)

initInput :: Int -> V.Vector Elf
initInput p = V.zip (V.enumFromN 0 p) (V.replicate p 1)

takeTurn :: Elf -> V.Vector Elf -> (Elf, Elf)
takeTurn (pos, pres) es =
    if pres == 0
    then ((pos, pres), (pos, pres))
    else let (front, back) = V.splitAt pos es
             (pos', pres') = V.head $
                 V.filter hasPresents (V.concat [ back, front ])
         in
             ((pos, pres + pres'), (pos, 0))

hasPresents :: Elf -> Bool
hasPresents (_, pr) = pr /= 0

activePlayers :: V.Vector Elf -> Int
activePlayers = V.length . V.filter hasPresents

elfPos :: Elf -> Int
elfPos (pos, _) = pos

turn :: State ElfState Int
turn = do
    (elfNum, es) <- get
    if (activePlayers es == 1)
        then return $ (elfPos $ V.head $ V.filter hasPresents es) + 1
        else let e@(pos, pres) = es V.! elfNum
                 nextElf = if elfNum == V.length es - 1 then 0 else elfNum + 1
             in
                 if hasPresents e
                 then let (front, back) = V.splitAt (elfNum + 1) es
                          (pos', pres') = V.head $
                              V.filter hasPresents (V.concat [ back, front ])
                          updateVec = [ (pos, (pos, pres + pres'))
                                      , (pos', (pos', 0))
                                      ]
                          newEs = es V.// updateVec
                      in do
                          put (nextElf, newEs)
                          turn
                 else do
                     put (nextElf, es)
                     turn

runGame :: Int -> Int
runGame players = evalState turn (0, initInput players)

turnFilter :: State (V.Vector Elf) Int
turnFilter = do
    es <- get
    if V.length es == 1 then
        return $ 1 + elfPos (V.head es)
        else let (two, rest) = V.splitAt 2 es
                 [(pos, pres), (_, pres')] = V.toList two
                 newE = (pos, pres + pres')
             in do put $ snoc rest newE
                   turnFilter

runFilteredGame :: Int -> Int
runFilteredGame players = evalState turnFilter (initInput players)


-- turnSeq :: State (S.Seq Elf) Int
-- turnSeq = do
--     es <- get
--     if S.length es == 1
--         then let x :< _ = S.viewl es
--              in return $ 1 + elfPos x
--         else let e :< _ = S.viewl

modTurn :: State [Int] Int
modTurn = do
    es <- get
    if Prelude.length es == 1
        then return $ Prelude.head es
        else let idx = Prelude.zip [0..] es
                 flt = Prelude.filter (odd . fst) idx
             in do put (Prelude.map snd flt)
                   modTurn
runModTurn :: Int -> Int
runModTurn players = evalState modTurn [1,3..players]


partTwo :: [Int] -> Int
partTwo [x] = x
partTwo xs  = partTwo $ partTwoHelp xs

partTwoHelp :: [Int] -> [Int]
partTwoHelp (x:xs) = let a = quot (Prelude.length xs) 2 - 1
                         (front, _:ys) = Prelude.splitAt a xs
    in front Prelude.++ ys Prelude.++ [x]
