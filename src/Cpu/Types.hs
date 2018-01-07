{-# LANGUAGE TemplateHaskell, DeriveGeneric, FlexibleContexts #-}

module Cpu.Types
    ( module BasicTypes

    , InvalidOpcode(..)
    , InvalidAddress(..)

    , Memory()

    , Register(..)
    , a
    , x
    , l
    , pc
    , sw

    , Device()

    , ST(..)
--  , register
    , memory
    , device

    , Opcode(..)

    , getByte
    , getBytes
    , getWord
    , getFWord

    , setByte
    , setBytes
    , setWord
    , setFWord

    , readByte
    , writeByte

    , viewR
    , overR
    , setR

    , fixedMemory
    , emptyRegister
    , cpuState
    ) where


import Prelude hiding (Word)

import GHC.Generics (Generic)

import System.IO (Handle, stdin, stdout)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import Data.Bits
import Data.Hashable (Hashable)
import Control.Exception
import Control.Lens

import BasicTypes


-- Exceptions
data InvalidOpcode = InvalidOpcode Word deriving Show
instance Exception InvalidOpcode

data InvalidAddress
    = InvalidAddress Address
    | InvalidAddressRange Address Int
    deriving Show
instance Exception InvalidAddress


-- Cpu data types
newtype Memory = Memory (Vector Byte) deriving Show

data Register = Register
    { _a :: Word    -- 0
    , _x :: Word    -- 1
    , _l :: Word    -- 2
    , _pc :: Word   -- 8
    , _sw :: Word   -- 9
    {-
    , _b :: Word    -- 3
    , _s :: Word    -- 4
    , _t :: Word    -- 5
    , _f :: FWord   -- 6
    -}
    } deriving Show

data Device = Device Handle Handle

data ST = ST
    { _memory   :: Memory
    , _register :: Register
    , _device   :: Byte -> Device
    }

makeLenses ''ST
makeLenses ''Register

data Opcode
    = LDA
    | LDX
    | LDL
    | STA
    | STX
    | STL
    | ADD
    | SUB
    | MUL
    | DIV
    | COMP
    | TIX
    | JEQ
    | JGT
    | JLT
    | J
    | AND
    | OR
    | JSUB
    | RSUB
    | LDCH
    | STCH
    | RD
    | WD
    | TD
    | STSW
    deriving (Show, Read, Eq, Generic)

instance Hashable Opcode

-- lossy conversion between data types (loss of data with overflow)
--convert :: (Integral a, Integral b, Bits a, Bits b) => a -> b
--convert = fromBytes . toBytes


adr :: Address -> Int
adr = fromIntegral

-- Low level getters
getByte :: Address -> Memory -> Byte
getByte addr (Memory mem) = case mem V.!? (adr addr) of
    Nothing -> throw $ InvalidAddress addr
    Just x  -> x

-- assume big endian
getBytes :: Address -> Int -> Memory -> [Byte]
getBytes addr n (Memory mem)
    | addr < 0 || V.length mem <= (adr addr + n) =
        throw $ InvalidAddressRange addr n
    | otherwise = V.toList . V.take n . V.drop (adr addr) $ mem


-- Low level setters
setByte :: Address -> Byte -> Memory -> Memory
setByte addr byte (Memory mem)
    | addr < 0 || V.length mem <= adr addr =
        throw $ InvalidAddress addr
    | otherwise = Memory . V.unsafeUpd mem $ [(adr addr, byte)]

-- assume big endian
setBytes :: Address -> [Byte] -> Memory -> Memory
setBytes addr bs (Memory mem)
    | addr < 0 || V.length mem <= (adr addr + length bs) =
        throw $ InvalidAddressRange addr (length bs)
    | otherwise = let addrs = [adr addr .. adr addr + length bs -1]
                  in Memory . V.unsafeUpd mem $ zip addrs bs


-- High level getter/setter

-- word is 3 bytes
getWord :: Address -> Memory -> Word
getWord addr mem = compose 8 $ getBytes addr 3 mem

-- fword is 6 bytes
getFWord :: Address -> Memory -> FWord
getFWord addr mem = compose 8 $ getBytes addr 6 mem

-- word is 3 bytes
setWord :: Address -> Word -> Memory -> Memory
setWord addr word = setBytes addr (decomposeBottom 3 8 word)

-- fword is 6 bytes
setFWord :: Address -> FWord -> Memory -> Memory
setFWord addr fword = setBytes addr (decomposeBottom 6 8 fword)


-- device IO

readByte :: Device -> IO Byte
readByte (Device h _) = B.head <$> B.hGet h 1

writeByte :: Device -> Byte -> IO ()
writeByte (Device _ h) b = B.hPut h $ B.singleton b


-- Register access

-- hide `register` lens and control the access manually
viewR reg = view (register . reg)
overR reg f = over (register.reg) $ (0xFFFFFF .&.) . f
setR reg val = set (register.reg) $ 0xFFFFFF .&. val


-- Constructors

-- fixed sized linear memory
fixedMemory :: Int -> Memory
fixedMemory n = Memory $ V.replicate n 0

emptyRegister :: Register
emptyRegister = Register 0 0 0 0 0

cpuState :: Memory -> [(Byte,Device)] -> ST
cpuState mem ds = ST
    { _memory   = mem
    , _register = emptyRegister
    , _device   = const (Device stdin stdout)
    }
