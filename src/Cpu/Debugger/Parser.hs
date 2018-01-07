
module Cpu.Debugger.Parser where


import Prelude hiding (Word)

import BasicTypes
import Cpu.Types hiding (memory)
import Cpu.Debugger.Types


parse :: String -> Maybe Command
parse "" = Just Empty
parse str = let (to:kens) = words str in case to of
    "mem" -> memory kens
    _ -> Nothing

memory :: [String] -> Maybe Command
memory ["-", str] = Mempick False <$> address str
memory ['-': str] = Mempick False <$> address str
memory ["+", str] = Mempick True  <$> address str
memory ['+': str] = Mempick True  <$> address str
memory [     str] = Mempick True  <$> address str
memory _ = Nothing

address :: String -> Maybe Address
address str = case reads $ "Word 0x" ++ str of
    [(addr,"")] -> Just $ addr
    _ -> Nothing
