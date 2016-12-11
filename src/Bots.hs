module Bots where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Text.Trifecta

testInput = "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"

data Bucket = Output Chip
            | ReadyBot Chip Chip
            | WaitingBot Chip
            | EmptyBot
    deriving (Eq, Show)

newtype Chip = Chip Integer
    deriving (Eq, Ord, Show)

newtype Status = Status (M.Map Target Bucket)
    deriving Show

data Target = OutputId Integer
            | BotId Integer
    deriving (Eq, Ord, Show)

data Command = Value Chip Target
             | Give Target Target Target
    deriving Show

partOne :: String -> M.Map Target Bucket
partOne input = let (Status finalState) = runInstructions (parseCommands input)
                in M.filter (== ReadyBot (Chip 17) (Chip 61)) finalState

partTwo :: String -> [Maybe Bucket]
partTwo input = let (Status finalState) = runInstructions (parseCommands input)
                in [M.lookup (OutputId 0) finalState,
                    M.lookup (OutputId 1) finalState,
                    M.lookup (OutputId 2) finalState]

runInstructions :: [Command] -> Status
runInstructions cs = runInstructionsHelp cs (Status M.empty)
  where
    runInstructionsHelp cmds status =
        let (unexec, s) = executeInstructions [] status cmds
        in
            case unexec of
                [] -> s
                _  -> runInstructionsHelp unexec s

executeInstructions :: [Command] -> Status -> [Command] -> ([Command], Status)
executeInstructions unexec status [] = (unexec, status)
executeInstructions unExec s (c:cs) =
    case executeInstruction c s of
        Nothing   -> executeInstructions (c:unExec) s cs
        Just newS -> executeInstructions unExec newS cs

executeInstruction :: Command -> Status -> Maybe Status
executeInstruction (Value c t) (Status status) =
    let newVal = give t c (Status status)
    in
        Just $ Status $ M.insert t newVal status
executeInstruction (Give from lT hT) (Status status) =
    case M.lookup from status of
        Nothing -> Nothing
        Just (EmptyBot) -> Nothing
        Just (WaitingBot _) -> Nothing
        Just (Output _) -> Nothing
        Just (ReadyBot lV hV) ->
            let lB = give lT lV (Status status)
                hB = give hT hV (Status status)
            in
                Just $ Status $ M.insert lT lB (M.insert hT hB status)

low :: Bucket -> Maybe Chip
low (ReadyBot l _) = Just l
low _              = Nothing

high :: Bucket -> Maybe Chip
high (ReadyBot _ h) = Just h
high _              = Nothing

give :: Target -> Chip -> Status -> Bucket
give t@(OutputId _) c (Status status) =
    fromMaybe (Output c) (M.lookup t status)
give t@(BotId _) c (Status status) =
    let bot = fromMaybe EmptyBot (M.lookup t status)
    in
        giveBot bot c

giveBot :: Bucket -> Chip -> Bucket
giveBot EmptyBot x         = WaitingBot x
giveBot (WaitingBot x) y   = if x > y then ReadyBot y x else ReadyBot x y
giveBot b@(ReadyBot _ _) _ = b
giveBot (Output x ) _      = Output x


-- Parsing machinery

parseCommands :: String -> [Command]
parseCommands input = case traverse (parseString commandParser mempty)
                                    (lines input) of
    Success x -> x
    Failure _ -> []

commandParser :: Parser Command
commandParser = valueParser <|> giveParser

valueParser :: Parser Command
valueParser = do
    _ <- string "value "
    c <- decimal
    _ <- string " goes to bot "
    b <- decimal
    return $ Value (Chip c) (BotId b)

giveParser :: Parser Command
giveParser = do
    _ <- string "bot "
    giver <- decimal
    _ <- string " gives low to "
    lt <- targetParser
    _ <- string " and high to "
    ht <- targetParser
    return $ Give (BotId giver) lt ht

targetParser :: Parser Target
targetParser = botParser <|> outputParser

botParser :: Parser Target
botParser = do
    _ <- string "bot "
    n <- decimal
    return $ BotId n

outputParser :: Parser Target
outputParser = do
    _ <- string "output "
    n <- decimal
    return $ OutputId n
