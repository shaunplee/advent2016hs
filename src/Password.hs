{-# LANGUAGE OverloadedStrings #-}

module Password where

import           Crypto.Hash
import qualified Data.ByteString.Lazy      as LB
import           Data.ByteString.Lazy.UTF8 (fromString)
import           Data.Char
import           Data.Map.Strict

md5 :: LB.ByteString -> Digest MD5
md5 = hashlazy

secret :: LB.ByteString
secret = "iwrupvqb"

findPassword :: String -> String
findPassword sec = take 8 $ mine (fromString sec) 0

mine :: LB.ByteString -> Integer -> String
mine secr i = let (x, c) = mineRecur secr i
            in x : (mine secr c)

mineRecur :: LB.ByteString -> Integer -> (Char, Integer)
mineRecur sec c =
    let try = LB.append sec (fromString $ show c)
    in case show $ md5 try of
        ('0':'0':'0':'0':'0':x:_) -> (x, c + 1)
        _                         -> mineRecur sec (c + 1)

-- Part 2
findAdvPassword :: String -> String
findAdvPassword sec = Prelude.map snd $ toList $ minePassword sec 0 empty

minePassword :: String -> Integer -> Map Integer Char -> Map Integer Char
minePassword sec c state =
    if size state == 8
    then state
    else let (pos, x, cNew) = mineRecurTwo (fromString sec) c
             newState = if member pos state then state else insert pos x state
         in
             minePassword sec cNew newState

mineRecurTwo :: LB.ByteString -> Integer -> (Integer, Char, Integer)
mineRecurTwo sec c = let try = LB.append sec (fromString $ show c)
                     in
                         case show $ md5 try of
                             ('0' : '0' : '0' : '0' : '0' : p : x : _) ->
                                 if isDigit p
                                 then let pos = read [ p ]
                                      in
                                          if pos < 8
                                          then (pos, x, c + 1)
                                          else mineRecurTwo sec (c + 1)
                                 else mineRecurTwo sec (c + 1)
                             _ -> mineRecurTwo sec (c + 1)
