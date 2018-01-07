{-# LANGUAGE TemplateHaskell #-}

module Assembler.Types where


import Control.Exception
import Control.Lens
import Data.HashMap.Lazy
import Data.ByteString (ByteString)

import BasicTypes
import Cpu.Types (Opcode)


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

data Target
    = Symbol ByteString
    | Value Int
    deriving Show

data Operand = Operand Target Bool deriving Show

data Pragma
    = BYTE ByteString Int
    | WORD ByteString Int
    | RESB ByteString Int
    | RESW ByteString Int
    deriving Show

data Line
    = Command ByteString Opcode Operand
    | Pragma Pragma
    deriving Show

data Assembly = Assembly ByteString Int Target [Line] deriving Show
--             name       start   boot   body


type LOCCTR = Int
type SymTbl = HashMap ByteString LOCCTR

data ST = ST
    { _loc :: LOCCTR
    , _sym :: SymTbl
    } deriving Show

makeLenses ''ST

data Result
    = ObjectCode [Byte]
    | Allocate Int
    deriving Show
