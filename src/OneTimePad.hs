{-# LANGUAGE OverloadedStrings #-}

module OneTimePad where

import           Crypto.Hash
import qualified Data.ByteString.Lazy      as LB
import           Data.ByteString.Lazy.UTF8 (fromString)
import           Data.List                 (zip5)
import           Data.Maybe                (mapMaybe)
import qualified Data.Set                  as S

import           Debug.Trace               (trace)

md5 :: LB.ByteString -> Digest MD5
md5 = hashlazy

salt :: String
salt = "qzyelonm"

testSalt :: String
testSalt = "abc"

findTriple :: (Int, String) -> Maybe (Int, Char, String)
findTriple (i, s) = let trips = zip3 s (drop 1 s) (drop 2 s)
               in
                   case filter (\(x, y, z) -> x == y && y == z) trips of
                       []               -> Nothing
                       ((x, _, _) : xs) -> Just (i, x, s)

-- hashes :: [String]
-- hashes = map (\x -> show $ md5 $ fromString (salt ++ show x)) [0..]

hashes :: [String]
hashes = map (\x -> show $ stretchedMd5 $ fromString (salt ++ show x))
                      [0 ..]

stretchedMd5 :: LB.ByteString -> LB.ByteString
stretchedMd5 s = iterate (fromString . show . md5) s !! 2017

indexedHashes :: [(Int, String)]
indexedHashes = zip [0..] hashes

indexedCandidates :: [(Int, Char, String, S.Set Char)]
indexedCandidates = countFives $ mapMaybe findTriple indexedHashes

countFives :: [(Int, Char, String)] -> [(Int, Char, String, S.Set Char)]
countFives = map countFive

countFive :: (Int, Char, String) -> (Int, Char, String, S.Set Char)
countFive (i, c, s) = let fives = zip5 s
                                       (drop 1 s)
                                       (drop 2 s)
                                       (drop 3 s)
                                       (drop 4 s)
                          allEq (v, w, x, y, z) = if v == w &&
                                                     w == x &&
                                                     x == y &&
                                                     y == z
                                                  then Just x
                                                  else Nothing
                      in
                          (i, c, s, S.fromList $ mapMaybe allEq fives)

keys :: [(Int, Char, String, S.Set Char)]
keys = keysHelp indexedCandidates
  where
    keysHelp (cand@(i, c, s, m) : cs) =
        let thous = takeWhile (\(t, _, _, _) -> t < i + 1000) cs
            fives = take 1 $ filter (\(_, _, _, f) -> S.member c f) thous
        in
            if not $ null fives
            then  cand : keysHelp cs
            else keysHelp cs
