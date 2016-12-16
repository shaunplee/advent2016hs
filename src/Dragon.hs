module Dragon where

--import           Data.Vector as V
import           Data.Maybe (fromMaybe)

newtype Bits = Bits [Bool]

instance Show Bits where
    show (Bits a) = map (\x -> if x then '1' else '0') a

instance Monoid Bits where
    mempty = Bits []
    mappend (Bits x) (Bits y) = Bits (x ++ y)

expandOne :: Bits -> Bits
expandOne (Bits a) = let b = reverse $ map not a
    in Bits $ concat [a, [False], b]

fromString :: String -> Maybe Bits
fromString s = do
    xs <- traverse (\x -> case x of
                              '0' -> Just False
                              '1' -> Just True
                              _   -> Nothing)
                         s
    return $ Bits xs

expandToFill :: Bits -> Int -> Bits
expandToFill b@(Bits inputBits) diskSize =
    if length inputBits >= diskSize
    then Bits $ take diskSize inputBits
    else expandToFill (expandOne b) diskSize

reduction :: Bits -> Bits
reduction (Bits []) = mempty
reduction (Bits xs) = let ([x,y], ys) = splitAt 2 xs
                          nv = x == y
                      in mappend (Bits [nv]) (reduction $ Bits ys)

checksum :: Bits -> Bits
checksum b = checksumHelp (reduction b)
  where
    checksumHelp c@(Bits inputBits) =
        if odd $ length inputBits then c else checksumHelp (reduction c)

partTwo :: String -> Bits
partTwo s = let a = fromMaybe (Bits [False]) (fromString "10111100110001111")
            in checksum $ expandToFill a 35651584
