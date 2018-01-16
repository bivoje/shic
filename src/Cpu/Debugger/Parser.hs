
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
    _ -> Nothing

-- | Parse arguments of memory command.
memory :: [String] -> Maybe Command
memory ["-", str] = Mempick False <$> address str
memory ['-': str] = Mempick False <$> address str
memory ["+", str] = Mempick True  <$> address str
memory ['+': str] = Mempick True  <$> address str
memory [     str] = Mempick True  <$> address str
memory _ = Nothing

-- | Parse a address.
address :: String -> Maybe Address
address str = case reads $ "Word 0x" ++ str of
    [(addr,"")] -> Just $ addr
    _ -> Nothing
