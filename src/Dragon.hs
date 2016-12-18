module Dragon where

import           Data.Maybe          (fromMaybe)
import           Data.Vector.Unboxed as V

import           Debug.Trace         (trace)

newtype Bits = Bits [Bool]

instance Show Bits where
    show (Bits a) = Prelude.map (\x -> if x then '1' else '0') a

instance Monoid Bits where
    mempty = Bits mempty
    mappend (Bits x) (Bits y) = Bits (x Prelude.++ y)

expandOne :: Bits -> Bits
expandOne (Bits a) = let b = Prelude.reverse $ Prelude.map not a
    in Bits $ Prelude.concat [a, False : b]

fromString :: String -> Maybe Bits
fromString s = do
    xs <- traverse (\x -> case x of
                              '0' -> Just False
                              '1' -> Just True
                              _   -> Nothing)
                         s
    return $ Bits  xs

expandToFill :: Bits -> Int -> Bits
expandToFill b@(Bits inputBits) diskSize =
    if Prelude.length inputBits >= diskSize
    then Bits $ Prelude.take diskSize inputBits
    else expandToFill (expandOne b) diskSize

reduction :: Bits -> Bits
reduction (Bits xs) = Bits $ reductionHelp xs
    where reductionHelp []       = []
          reductionHelp (x:y:ys) = (x == y) : reductionHelp ys

--checksum :: Bits -> Bits
--checksum b = checksumHelp (reduction b)
  -- where
  --   checksumHelp c@(Bits inputBits) =
  --       if odd $ V.length inputBits then c else trace (show $ V.length inputBits) checksumHelp (reduction c)

checksum :: Bits -> Bits
checksum b = Prelude.head $ Prelude.dropWhile (\(Bits x) -> trace (show $ Prelude.length x) (even $ Prelude.length x)) (iterate reduction b)

partTwo :: Bits
partTwo = let a = fromMaybe (Bits [False])
                            (fromString "10111100110001111")
          in
              checksum $ expandToFill a 35651584

-- partTwo :: String
-- partTwo = fill 35651584 "10111100110001111"

expandData :: String -> String
expandData a = a Prelude.++ "0" Prelude.++ (dNot <$> Prelude.reverse a)
    where
        dNot '0' = '1'
        dNot '1' = '0'
        dNot _   = error "bad dNot arg"

fill :: Int -> String -> String
fill len input = cs rawData
    where rawData = Prelude.take len minExpansion
          minExpansion = Prelude.head $ Prelude.dropWhile ((<= len) . Prelude.length) expansions
          expansions = iterate expandData input

cs :: String -> String
cs xs = Prelude.head $
    Prelude.dropWhile (even . Prelude.length) $ iterate go xs
  where
    go ys@(a : b : _) = (if a == b then '1' else '0') : go (Prelude.drop 2 ys)
    go _              = []
