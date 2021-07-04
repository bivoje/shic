
-- | Parser for command input of debugger.
--
-- This module does not utilize monadic parsers.
-- It uses __primitive__ way.
module Cpu.Debugger.Parser where


import Prelude hiding (Word)

import BasicTypes
import Cpu.Types hiding (memory)
import Cpu.Debugger.Types


-- | Parse a command.
parse :: String -> Maybe Command
parse "" = Just Empty
parse str = let (to:kens) = words str in case to of
    "mem" -> memory kens
    "quit" -> Just Exit
    "n" -> next kens
    "b" -> breakAt kens
    "w" -> watch kens
    _ -> Nothing

-- | Parse arguments of memory command.
memory :: [String] -> Maybe Command
memory ["-", str] = Mempick False <$> address str
memory ['-': str] = Mempick False <$> address str
memory ["+", str] = Mempick True  <$> address str
memory ['+': str] = Mempick True  <$> address str
memory [     str] = Mempick True  <$> address str
memory _ = Nothing

next :: [String] -> Maybe Command
next [] = Just (Step 1)
next [s] = Step <$> natural s
next _ = Nothing

breakAt :: [String] -> Maybe Command
breakAt [] = Just (BreakAt Nothing)
breakAt [s] = BreakAt . Just <$> address s

watch :: [String] -> Maybe Command
watch [r, "==", x] = WatchWReg True <$> reg r <*> num x
watch [r, "!=", x] = WatchWReg False <$> reg r <*> num x
watch _ = Nothing

-- | Parse a address.
address :: String -> Maybe Address
address str = case reads $ "0x" ++ str of
    [(addr,"")] -> Just addr
    _ -> Nothing

natural :: String -> Maybe Int
natural str = case reads str of
    [(nat,"")] | nat > 0 -> Just $ nat
    _ -> Nothing

num :: String -> Maybe Word
num str = case reads str of
    [(n,"")] -> Just $ n
    _ -> Nothing

reg :: String -> Maybe Int
reg "a"  = Just 0
reg "x"  = Just 1
reg "l"  = Just 2
reg "pc" = Just 8
reg "sw" = Just 9
reg _ = Nothing
