module Safe where

import           Control.Applicative
import           Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import           Text.Trifecta

data Command = Cpy Expr Expr
             | Inc Expr
             | Dec Expr
             | Jnz Expr Expr
             | Tgl Expr
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

newtype Instructions = Instructions (V.Vector Command)
    deriving Show

newtype State = State (Integer, Registers, Instructions)
    deriving Show

initRegisters :: Registers
initRegisters = Registers $ M.fromList [(A, 12), (B, 0), (C, 0), (D, 0)]

partOne :: String -> State
partOne input = let cs = Instructions $
                         fmap parseLine (V.fromList (lines input))
                in
                    runCommands (State (0, initRegisters, cs))

readValue :: Registers -> Expr -> Integer
readValue _ (Value x)                 = x
readValue (Registers rs) (Register r) = fromMaybe 0 $ M.lookup r rs

runCommands :: State -> State
runCommands s@(State (pc, rs, Instructions cs)) =
    if fromInteger pc >= V.length cs
    then s
    else runCommands (runNextCommand s)

runNextCommand :: State -> State
runNextCommand s@(State (pc, rs, i@(Instructions cs))) =
    let cmd = cs V.! fromInteger pc
    in
        case cmd of
            Cpy x y -> State (pc + 1, execCopy x y rs, i)
            Inc x -> State (pc + 1, execInc x rs, i)
            Dec x -> State (pc + 1, execDec x rs, i)
            Jnz x y -> let jump = execJnz x y rs
                       in
                           State (pc + jump, rs, i)
            Tgl x -> execTgl x s
            Noop -> State (pc + 1, rs, i)

execCopy :: Expr -> Expr -> Registers -> Registers
execCopy v (Register r) (Registers rs) =
    let newV = case v of
            Value x     -> x
            Register rx -> fromMaybe 0 (M.lookup rx rs)
    in
        Registers $ M.insert r newV rs
execCopy _ (Value _) rs = rs

execInc :: Expr -> Registers -> Registers
execInc (Register r) (Registers rs) =
    case M.lookup r rs of
        Nothing -> Registers rs
        Just _  -> Registers (insertWith (+) r 1 rs)
execInc (Value _) rs = rs

execDec :: Expr -> Registers -> Registers
execDec (Register r) (Registers rs) =
    case M.lookup r rs of
        Nothing -> Registers rs
        Just _  -> Registers (insertWith subtract r 1 rs)
execDec (Value _) rs = rs

execJnz :: Expr -> Expr -> Registers -> Integer
execJnz (Value x) jump rs = if x == 0 then 1 else readValue rs jump
execJnz (Register r) jump (Registers rs) =
    case M.lookup r rs of
        Nothing -> 1
        Just x  -> if x == 0 then 1 else readValue (Registers rs) jump

execTgl :: Expr -> State -> State
execTgl x (State (pc, Registers rs, Instructions cs)) =
    let offset = case x of
            Value o -> o
            Register r -> case M.lookup r rs of
                Nothing -> error $ "register " ++ show r ++ " invalid"
                Just rr -> rr
    in
        case cs V.!? fromInteger (pc + offset) of
            Nothing -> State (pc + 1, Registers rs, Instructions cs)
            Just target -> let newTarget = tglCmd target
                               newCs = cs V.//
                                   [ (fromInteger (pc + offset), newTarget) ]
                           in
                               State (pc + 1, Registers rs, Instructions newCs)

tglCmd :: Command -> Command
tglCmd (Inc x)   = Dec x
tglCmd (Dec x)   = Inc x
tglCmd (Tgl x)   = Inc x
tglCmd (Cpy x y) = Jnz x y
tglCmd (Jnz x y) = Cpy x y

parseLine :: String -> Command
parseLine input = case parseString (copyParser <|>
                                        incParser <|>
                                        decParser <|>
                                        jnzParser <|>
                                        tglParser)
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
    x <- integer
    return $ Value x

twoArgParser :: Parser (Expr, Expr)
twoArgParser = do
    x <- exprParser
    _ <- many space
    y <- exprParser
    return (x, y)

copyParser :: Parser Command
copyParser = do
    _ <- string "cpy "
    (x,y) <- twoArgParser
    return $ Cpy x y

incParser :: Parser Command
incParser = do
    _ <- string "inc "
    x <- exprParser
    return $ Inc x

decParser :: Parser Command
decParser = do
    _ <- string "dec "
    x <- exprParser
    return $ Dec x

jnzParser :: Parser Command
jnzParser = do
    _ <- string "jnz "
    (x,y) <- twoArgParser
    return $ Jnz x y

tglParser :: Parser Command
tglParser = do
    _ <- string "tgl "
    x <- exprParser
    return $ Tgl x

inputData = "cpy 1 a\ncpy 1 b\ncpy 26 d\njnz c 2\njnz 1 5\ncpy 7 c\ninc d\ndec c\njnz c -2\ncpy a c\ninc a\ndec b\njnz b -2\ncpy c b\ndec d\njnz d -6\ncpy 18 c\ncpy 11 d\ninc a\ndec d\njnz d -2\ndec c\njnz c -5"

testInput = "cpy -2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
