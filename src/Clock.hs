module Clock where

import           Control.Applicative
import           Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import           Debug.Trace
import           Text.Trifecta

data Command = Cpy Expr Expr
             | Inc Expr
             | Dec Expr
             | Jnz Expr Expr
             | Mul Expr Expr
             | Add Expr Expr
             | Out Expr
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

newtype State = State (Integer, Registers, Instructions, Maybe Integer)
    deriving Show

partOne :: Integer
partOne = head $ Prelude.filter (runTest input) [0..]

runTest :: Instructions -> Integer -> Bool
runTest ins a = let run = runNOuts 100 (initState a ins)
                in
                    run == take 100 desired

optInput :: Instructions
optInput = Instructions $ fmap parseLine (V.fromList (lines inputCmds))

input :: Instructions
input = Instructions $ fmap parseLine (V.fromList (lines origInputCmds))

initRegisters :: Integer -> Registers
initRegisters x = Registers $ M.fromList [(A, x), (B, 0), (C, 0), (D, 0)]

initState :: Integer -> Instructions -> State
initState a is = State (0, initRegisters a, is, Nothing)

-- partOne :: Integer
-- partOne input = let cs = Instructions $
--                          fmap parseLine (V.fromList (lines input))
--                 in
--                     runCommands (State (0, initRegisters, cs))

readValue :: Registers -> Expr -> Integer
readValue _ (Value x)                 = x
readValue (Registers rs) (Register r) = fromMaybe 0 $ M.lookup r rs

runCommands :: State -> State
runCommands s@(State (pc, rs, Instructions cs, out)) =
    if fromInteger pc >= V.length cs
    then s
    else runCommands (runNextCommand s)

runNSteps :: Int -> State -> State
runNSteps 0 is = is
runNSteps n is = runNSteps (n - 1) (runNextCommand is)

runNOuts :: Int -> State -> [Integer]
runNOuts 0 _ = []
runNOuts n is@(State (_, _, _, Nothing)) = runNOuts n (runNextCommand is)
runNOuts n is@(State (pc, rs, cs, Just x)) =
    x : runNOuts (n - 1) (runNextCommand (State (pc, rs, cs, Nothing)))

runNextCommand :: State -> State
runNextCommand s@(State (pc, rs, i@(Instructions cs), Nothing)) =
    let cmd = cs V.! fromInteger pc
    in
        case cmd of
            Cpy x y -> State (pc + 1, execCopy x y rs, i, Nothing)
            Inc x -> State (pc + 1, execInc x rs, i, Nothing)
            Dec x -> State (pc + 1, execDec x rs, i, Nothing)
            Jnz x y -> let jump = execJnz x y rs
                       in
                           State (pc + jump, rs, i, Nothing)
            Mul x y -> State (pc + 1, execMul x y rs, i, Nothing)
            Add x y -> State (pc + 1, execAdd x y rs, i, Nothing)
            Out x -> State (pc + 1, rs, i, Just (get x rs))
            Noop -> State (pc + 1, rs, i, Nothing)

get :: Expr -> Registers -> Integer
get (Value v) _                 = v
get (Register r) (Registers rs) = fromMaybe (-1) (M.lookup r rs)

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

execMul :: Expr -> Expr -> Registers -> Registers
execMul (Register r) (Register s) (Registers rs) =
    let newVal = do x <- M.lookup r rs
                    y <- M.lookup s rs
                    return $ x * y
    in case newVal of
        Nothing -> error "register not found"
        Just x  -> Registers (M.insert r x rs)
execMul (Register r) (Value v) (Registers rs) =
    case M.lookup r rs of
        Nothing -> error "register not found"
        Just x  -> Registers $ M.insert r (x * v) rs

execAdd :: Expr -> Expr -> Registers -> Registers
execAdd (Register r) (Register s) (Registers rs) =
    let newVal = do
            x <- M.lookup r rs
            y <- M.lookup s rs
            return $ x * y
    in
        case newVal of
            Nothing -> Registers rs
            Just x  -> Registers $ M.insert r x rs
execAdd (Register r) (Value v) (Registers rs) =
    case M.lookup r rs of
        Nothing -> Registers rs
        Just x  -> Registers $ M.insert r (x + v) rs

parseLine :: String -> Command
parseLine input = case parseString (copyParser <|>
                                        addParser <|>
                                        mulParser <|>
                                        incParser <|>
                                        decParser <|>
                                        jnzParser <|>
                                        outParser <|>
                                        noopParser)
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

outParser :: Parser Command
outParser = do
    _ <- string "out "
    x <- exprParser
    return $ Out x

addParser :: Parser Command
addParser = do
    _ <- string "add "
    (x, y) <- twoArgParser
    return $ Add x y

mulParser :: Parser Command
mulParser = do
    _ <- string "mul "
    (x, y) <- twoArgParser
    return $ Mul x y

noopParser :: Parser Command
noopParser = do
    _ <- string "nop"
    return Noop

origInputCmds :: String
origInputCmds = "cpy a d\ncpy 9 c\ncpy 282 b\ninc d\ndec b\njnz b -2\ndec c\njnz c -5\ncpy d a\njnz 0 0\ncpy a b\ncpy 0 a\ncpy 2 c\njnz b 2\njnz 1 6\ndec b\ndec c\njnz c -4\ninc a\njnz 1 -7\ncpy 2 b\njnz c 2\njnz 1 4\ndec b\ndec c\njnz 1 -4\njnz 0 0\nout b\njnz a -19\njnz 1 -21\n"

inputCmds :: String
inputCmds = "cpy a d\ncpy 9 c\nmul c 282\nnop\ncpy c d\nnop\ncpy 0 c\njnz c -5\ncpy d a\njnz 0 0\ncpy a b\ncpy 0 a\ncpy 2 c\njnz b 2\njnz 1 6\ndec b\ndec c\njnz c -4\ninc a\njnz 1 -7\ncpy 2 b\njnz c 2\njnz 1 4\ndec b\ndec c\njnz 1 -4\njnz 0 0\nout b\njnz a -19\njnz 1 -21\n"

desired :: [Integer]
desired = 0 : 1 : desired
