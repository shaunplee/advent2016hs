{-# LANGUAGE OverloadedStrings #-}
module Door where

import           Control.Applicative ((<|>))
import           Data.List           (foldl')
import           Data.Vector         as V
import           Text.Trifecta

data Command = Rect Int Int
    | Row Int Int
    | Col Int Int
    | Noop
    deriving Show

newtype Screen = Screen (Vector (Vector Bool))

testCommands = "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"

screenSizeX = 50
screenSizeY = 6

instance Show Screen where
    show (Screen scr) = Prelude.concat $ toList $ V.map flatRow scr
      where
        flatRow y = (toList $
                         V.map (\x -> if x then 'O' else '.') y) Prelude.++
            [ '\n' ]

countLightsOn :: String -> Int
countLightsOn input = countLit $ processCommands input

processCommands :: String -> Screen
processCommands input = let commands = Prelude.map parseLine (lines input)
    in Data.List.foldl' evalCommand initScreen commands

countLit :: Screen -> Int
countLit (Screen scr) = V.length $ V.filter id $ V.concat $ toList scr

parseLine :: String -> Command
parseLine input = case parseString (rectParse <|> rowParse <|> colParse)
                                   mempty
                                   input of
    Success x -> x
    Failure _ -> Noop

coordParse :: Parser (Int, Int)
coordParse = do
    a <- decimal
    _ <- string "x" <|> string " by "
    b <- decimal
    return (fromInteger a, fromInteger b)

rectParse :: Parser Command
rectParse = do
    _ <- string "rect "
    (a, b) <- coordParse
    return $ Rect a b

rowParse :: Parser Command
rowParse = do
    _ <- string "rotate row y="
    (a, b) <- coordParse
    return $ Row a b

colParse :: Parser Command
colParse = do
    _ <- string "rotate column x="
    (a, b) <- coordParse
    return $ Col a b

initScreen :: Screen
initScreen = Screen $ V.replicate screenSizeY (V.replicate screenSizeX False)

evalCommand :: Screen -> Command -> Screen
evalCommand (Screen scr) (Rect a b)=
    let updateRow i = let curRow = scr V.! i
                          rowUpdateV = V.zip (V.enumFromN 0 a)
                                             (V.replicate a True)
                      in
                          (i, update curRow rowUpdateV)
        rowUpdateV = V.map updateRow (V.enumFromN 0 b)
    in
        Screen $ V.update scr rowUpdateV
evalCommand (Screen scr) (Row a b) =
    let curRow = (scr V.! a)
        newRow = update curRow
                        (V.map (\i -> ((i + b) `mod` screenSizeX, curRow V.! i))
                               (V.enumFromN 0 screenSizeX))
    in
        Screen $ V.update scr (V.singleton (a, newRow))
evalCommand (Screen scr) (Col a b) =
    let updateRow i = let curRow = scr V.! i
                          prevRowI = if (i - b) < 0
                                     then screenSizeY + (i - b)
                                     else (i - b)
                          prevRow = scr V.! prevRowI
                      in
                          V.update curRow (V.singleton (a, prevRow V.! a))
    in Screen $ V.map updateRow (V.enumFromN 0 screenSizeY)
evalCommand scr Noop = scr
