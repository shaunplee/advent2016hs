module Main where

--import           Bathroom
import           System.Environment (getArgs)
import           Triangles
--import           Navigation

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    print $ Triangles.countValidTrianglesCols content
