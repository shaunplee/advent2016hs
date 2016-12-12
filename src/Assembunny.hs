module Assembunny where

import           Control.Applicative
import           Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Text.Trifecta

data Command = Cpy Expr Target
             | Inc Target
             | Dec Target
             | Jnz Expr Integer
             | Noop
    deriving Show

data Reg = A | B | C | D
    deriving (Eq, Ord, Show)

newtype Target = Target Reg
    deriving Show

data Expr = Register Reg
          | Value Integer
    deriving Show

newtype Registers = Registers (M.Map Reg Integer)
    deriving Show

newtype Instructions = Instructions [Command]
    deriving Show

newtype State = State (Integer, Registers)
    deriving Show

initRegisters :: Registers
initRegisters = Registers $ M.fromList [(A, 0), (B, 0), (C, 1), (D, 0)]

partOne :: String -> State
partOne input = runCommands (Prelude.map parseLine (lines input))
                            (State (0, initRegisters))

runCommands :: [Command] -> State -> State
runCommands cs s@(State (pc, rs)) =
    if fromInteger pc >= length cs
    then s
    else runCommands cs (runNextCommand cs s)

runNextCommand :: [Command] -> State -> State
runNextCommand cs s@(State (pc, rs)) =
    let cmd = cs !! fromInteger pc
    in
        case cmd of
            Cpy x y -> State (pc + 1, execCopy x y rs)
            Inc x -> State (pc + 1, execInc x rs)
            Dec x -> State (pc + 1, execDec x rs)
            Jnz x y -> let jump = execJnz x y rs
                       in
                           State (pc + jump, rs)

execCopy :: Expr -> Target -> Registers -> Registers
execCopy v (Target r) (Registers rs) =
    let newV = case v of
            Value x     -> x
            Register rx -> fromMaybe 0 (M.lookup rx rs)
    in
        Registers $ M.insert r newV rs

execInc :: Target -> Registers -> Registers
execInc (Target r) (Registers rs) =
    case M.lookup r rs of
        Nothing -> Registers rs
        Just _  -> Registers (insertWith (+) r 1 rs)

execDec :: Target -> Registers -> Registers
execDec (Target r) (Registers rs) =
    case M.lookup r rs of
        Nothing -> Registers rs
        Just _  -> Registers (insertWith subtract r 1 rs)

execJnz :: Expr -> Integer -> Registers -> Integer
execJnz (Value x) jump _ = if x == 0 then 1 else jump
execJnz (Register r) jump (Registers rs) =
    case M.lookup r rs of
        Nothing -> 1
        Just x  -> if x == 0 then 1 else jump

parseLine :: String -> Command
parseLine input = case parseString (copyParser <|> incParser <|> decParser <|>
                                        jnzParser)
                                   mempty
                                   input of
    Success x -> x
    Failure _ -> Noop

registerParser :: Parser Expr
registerParser = do
    ch <- char 'a' <|> char 'b' <|> char 'c' <|> char 'd'
    return $
        Register $
            case ch of
                'a' -> A
                'b' -> B
                'c' -> C
                'd' -> D

targetParser :: Parser Target
targetParser = do
    ch <- char 'a' <|> char 'b' <|> char 'c' <|> char 'd'
    return $
        Target $
            case ch of
                'a' -> A
                'b' -> B
                'c' -> C
                'd' -> D

exprParser :: Parser Expr
exprParser = valParser <|> registerParser

valParser :: Parser Expr
valParser = do
    x <- decimal
    return $ Value x

copyParser :: Parser Command
copyParser = do
    _ <- string "cpy "
    x <- exprParser
    _ <- space
    y <- targetParser
    return $ Cpy x y

incParser :: Parser Command
incParser = do
    _ <- string "inc "
    x <- targetParser
    return $ Inc x

decParser :: Parser Command
decParser = do
    _ <- string "dec "
    x <- targetParser
    return $ Dec x

jnzParser :: Parser Command
jnzParser = do
    _ <- string "jnz "
    x <- exprParser
    _ <- space
    y <- integer
    return $ Jnz x y

inputData = "cpy 1 a\ncpy 1 b\ncpy 26 d\njnz c 2\njnz 1 5\ncpy 7 c\ninc d\ndec c\njnz c -2\ncpy a c\ninc a\ndec b\njnz b -2\ncpy c b\ndec d\njnz d -6\ncpy 18 c\ncpy 11 d\ninc a\ndec d\njnz d -2\ndec c\njnz c -5"
