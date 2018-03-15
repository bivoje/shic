{-# LANGUAGE TemplateHaskell #-}

module Assembler.Types where

import Prelude hiding (Word)

import Control.Exception
import Control.Lens
import Data.HashMap.Strict
import Data.ByteString (ByteString)

import BasicTypes
import Cpu.Types (Opcode)


----------------------------------------------------------------------
-- * Parser Types

data Assembly = Assembly ByteString Address Target [Line] deriving Show
--                       name       start   boot   body

data Line
    = Command Symbol Opcode Operand
    | Pragma Pragma
    deriving Show

data Pragma
    = BYTE Symbol Byte
    | WORD Symbol Word
    | RESB Symbol Length
    | RESW Symbol Length
    deriving Show

data Operand = Operand Target Bool deriving Show

data Target
    = Symbol Symbol
    | Value Address
    deriving (Show, Eq)


----------------------------------------------------------------------
-- * Assembler Types

data Result
    = ObjectCode [Byte]
    | Allocate Length
    deriving Show

type LOCCTR = Word
type Symbol = ByteString
type SymTbl = HashMap Symbol LOCCTR

data ST = ST
    { _loc :: LOCCTR
    , _sym :: SymTbl
    } deriving (Show, Eq)

makeLenses ''ST


----------------------------------------------------------------------
-- * Exceptions

data DuplicatedSymbol = DuplicatedSymbol Symbol deriving (Show, Eq)
instance Exception DuplicatedSymbol

data ProgramTooBig = ProgramTooBig deriving (Show, Eq)
instance Exception ProgramTooBig

data InvalidReservation = InvalidReservation Length deriving (Show, Eq)
instance Exception InvalidReservation

data UnknownSymbol = UnknownSymbol Symbol deriving (Show, Eq)
instance Exception UnknownSymbol

data AddressTooLarge = AddressTooLarge Length deriving (Show, Eq)
instance Exception AddressTooLarge