module Navigation where

import           Control.Applicative
import           Text.Trifecta

navigateDistance :: String -> Integer
navigateDistance s = let inst = parseString parseInstructions mempty s
                     in
                         case inst of
                             Failure _ -> 0
                             Success inst -> distance $
                                 foldl move initState inst

newtype Direction a = Direction (a, a)
    deriving Show

instance Functor Direction  where
    fmap f (Direction (x, y)) = Direction (f x, f y)

newtype Position = Position (Integer, Integer)

newtype Distance = Distance Integer
    deriving Show

data Turn = LeftTurn | RightTurn
    deriving Show

data Instruction = Instruction Turn Distance
    deriving Show

data State = State (Direction Integer) Position

distance :: State -> Integer
distance (State _ (Position (x, y))) = (abs x) + (abs y)

initState :: State
initState = State (Direction (0, 1)) (Position (0, 0))

move :: State -> Instruction -> State
move (State dir (Position (x, y))) (Instruction t (Distance dist)) =
    let newD = turn t dir
        Direction (newx, newy) = (*dist) <$> dir
        newPos = Position (x + newx, y + newy)
    in State newD newPos

turn :: Turn -> Direction Integer -> Direction Integer
turn RightTurn (Direction (1, _))  = Direction (0, -1)
turn RightTurn (Direction (-1, _)) = Direction (0, 1)
turn RightTurn (Direction (_, 1))  = Direction (1, 0)
turn RightTurn (Direction (_, -1)) = Direction (-1, 0)
turn LeftTurn (Direction (1, _))   = Direction (0, 11)
turn LeftTurn (Direction (-1, _))  = Direction (0, -11)
turn LeftTurn (Direction (_, 1))   = Direction (-1, 0)
turn LeftTurn (Direction (_, -1))  = Direction (1, 0)

parseInstructions :: Parser [Instruction]
parseInstructions = do
    first <- parseInstruction
    rest <- some parseNextInstruction
    return $ first : rest

parseInstruction :: Parser Instruction
parseInstruction = do
    turn <- parseTurn
    dist <- parseDist
    return $ Instruction turn dist

parseNextInstruction :: Parser Instruction
parseNextInstruction = do
    _ <- string ", "
    parseInstruction

parseTurn :: Parser Turn
parseTurn = (char 'R' >>= \_ -> return RightTurn)
    <|> (char 'L' >>= \_ -> return LeftTurn)

parseDist :: Parser Distance
parseDist = decimal >>= \d -> return $ Distance d
