module Rooms where

import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Map
import           Text.Trifecta

data Room = Room Name Sector Checksum
    deriving Show

newtype Name = Name String
    deriving Show

newtype Sector = Sector Integer
    deriving Show

newtype Checksum = Checksum String
    deriving Show

sumSectorIdRealRooms :: String -> Integer
sumSectorIdRealRooms rs =
    sumSectors $ Data.List.filter validRoom $ parseInput rs

sumSectors :: [Room] -> Integer
sumSectors = Data.List.foldl' (\acc (Room _ (Sector s) _) -> acc + s) 0

parseInput :: String -> [Room]
parseInput rs = case traverse (parseString roomParser mempty) (lines rs) of
    Success x -> x
    Failure _ -> []

roomParser :: Parser Room
roomParser = do
    name <- nameParser
    sector <- sectorParser
    checksum <- checksumParser
    return $ Room name sector checksum

nameParser :: Parser Name
nameParser = do
    x <- some (letter <|> char '-')
    return $ Name x

sectorParser :: Parser Sector
sectorParser = do
    x <- decimal
    return $ Sector x

checksumParser :: Parser Checksum
checksumParser = do
    _ <- char '['
    c <- some letter
    _ <- char ']'
    return $ Checksum c

validRoom :: Room -> Bool
validRoom (Room (Name name) _ (Checksum checksum)) =
    let counts = toList $
            fromListWith (+)
                         [ (c, 1)
                         | c <- name ]
        sorted = sortOn (\(x, y) -> (-y, x))
                        (Data.List.filter (\(x, _) -> x /= '-') counts)
        cs = Prelude.map fst $ take 5 sorted
    in
        checksum == cs


-- Part 2
caesarChr :: Int -> Char -> Char
caesarChr _ '-' = ' '
caesarChr n c = chr (mod (ord c + n - ord base) 26 + ord base)
  where base = if isUpper c then 'A' else 'a'

caesar :: Int -> String -> String
caesar n = Data.List.map (caesarChr n)

decryptRoom :: Room -> Room
decryptRoom (Room (Name name) (Sector s) cs) =
    Room (Name (caesar (fromInteger s) name)) (Sector s) cs

decryptRealRooms :: String -> [Room]
decryptRealRooms rs = Data.List.map decryptRoom $
    Data.List.filter validRoom $ parseInput rs
