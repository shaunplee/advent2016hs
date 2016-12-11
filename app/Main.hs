module Main where

--import           Bathroom
--import           Bots
--import           Compression
--import           Door
import           Microchips
--import           Password
--import           Rooms
--import           Signal
--import           IP7
import           System.Environment (getArgs)
--import           Triangles
--import           Navigation

main :: IO ()
main = do
--    args <- getArgs
--    content <- readFile (head args)
--    print $ Password.findAdvPassword (head args)
    print $ Microchips.partOneRun
