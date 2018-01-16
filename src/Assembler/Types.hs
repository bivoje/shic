{-# LANGUAGE TemplateHaskell #-}

module Assembler.Types where


import Control.Exception
import Control.Lens
import Data.HashMap.Strict
import Data.ByteString (ByteString)

import BasicTypes
import Cpu.Types (Opcode)


----------------------------------------------------------------------
-- * Parser Types


data Assembly = Assembly ByteString Int Target [Line] deriving Show
--             name       start   boot   body

data Line
    = Command ByteString Opcode Operand
    | Pragma Pragma
    deriving Show

data Pragma
    = BYTE ByteString Int
    | WORD ByteString Int
    | RESB ByteString Int
    | RESW ByteString Int
    deriving Show

data Operand = Operand Target Bool deriving Show


----------------------------------------------------------------------
-- * Assembler Types

data Result
    = ObjectCode [Byte]
    | Allocate Int
    deriving Show

data Target
    = Symbol ByteString
    | Value Int
    deriving Show

type LOCCTR = Int
type SymTbl = HashMap ByteString LOCCTR

data ST = ST
    { _loc :: LOCCTR
    , _sym :: SymTbl
    } deriving Show

makeLenses ''ST


----------------------------------------------------------------------
-- * Exceptions

data DuplicatedSymbol = DuplicatedSymbol ByteString deriving Show
instance Exception DuplicatedSymbol

data ProgramTooBig = ProgramTooBig deriving Show
instance Exception ProgramTooBig

data InvalidReservation = InvalidReservation Int deriving Show
instance Exception InvalidReservation

data UnknownSymbol = UnknownSymbol ByteString deriving Show
instance Exception UnknownSymbol

data InvalidAddressing = InvalidAddressing Target deriving Show
instance Exception InvalidAddressing
