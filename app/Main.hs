module Main where

--import           Bathroom
import           Password
--import           Rooms
import           System.Environment (getArgs)
--import           Triangles
--import           Navigation

main :: IO ()
main = do
    args <- getArgs
--    content <- readFile (head args)
    print $ Password.findAdvPassword (head args)
