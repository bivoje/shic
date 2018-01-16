{-# LANGUAGE TemplateHaskell, DeriveGeneric, FlexibleContexts #-}

module Cpu.Types
    ( module BasicTypes

    -- * Operation Code
    , Opcode(..)
    , getOp
    , fromOp

    -- * Cpu State
    , ST(..)
    , cpuState

    -- ** Accessors
    , memory
    , device

    -- ** Register Accessors
    -- $registerAccessors
    , viewR
    , overR
    , setR

    -- * Memory
    , Memory()
    , fixedMemory

    -- ** Memory getters
    -- $memoryAccessors
    , getByte
    , getBytes
    , getWord
    , getFWord

    -- ** Memory setters
    , setByte
    , setBytes
    , setWord
    , setFWord

    -- * Register
    , Register(..)
    , emptyRegister

    -- ** Lenses
    , a
    , x
    , l
    , pc
    , sw

    -- * Device
    , Device()
    -- TODO CREAT DEVICE

    -- ** device IO
    , readByte
    , writeByte

    -- * Exceptions
    , InvalidOpcode(..)
    , InvalidAddress(..)

    ) where


import Prelude hiding (Word)

import GHC.Generics (Generic)

import System.IO (Handle, stdin, stdout)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.Bits
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as H
import Control.Exception
import Control.Lens

import BasicTypes

import Control.Monad.Reader (MonadReader)


----------------------------------------------------------------------
-- Exceptions

data InvalidOpcode = InvalidOpcode Word deriving Show
instance Exception InvalidOpcode

data InvalidAddress
    = InvalidAddress Address
    | InvalidAddressRange Address Int
    deriving Show
instance Exception InvalidAddress


----------------------------------------------------------------------
-- Cpu data types

-- | Memory for SIC machine.
newtype Memory = Memory (Vector Byte) deriving Show

-- | Register in SIC machine.
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

-- | In-N-Out device for SIC machine.
data Device = Device Handle Handle

-- | State for cpu running.
data ST = ST
    { _memory   :: Memory
    , _register :: Register
    , _device   :: Byte -> Device
    }

makeLenses ''ST
makeLenses ''Register

-- | Opcode for SIC instructions.
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

-- | Map Opcode to Byte representation
getOp :: Opcode -> Byte
getOp op = fromJust $ H.lookup op optbl
    where optbl :: HashMap Opcode Byte
          optbl = H.fromList
              [ (LDA  , 0x00)
              , (LDX  , 0x04)
              , (LDL  , 0x08)
              , (STA  , 0x0C)
              , (STX  , 0x10)
              , (STL  , 0x14)
              , (ADD  , 0x18)
              , (SUB  , 0x1C)
              , (MUL  , 0x20)
              , (DIV  , 0x24)
              , (COMP , 0x28)
              , (TIX  , 0x2C)
              , (JEQ  , 0x30)
              , (JGT  , 0x34)
              , (JLT  , 0x38)
              , (J    , 0x3C)
              , (AND  , 0x40)
              , (OR   , 0x44)
              , (JSUB , 0x48)
              , (RSUB , 0x4C)
              , (LDCH , 0x50)
              , (STCH , 0x54)
              , (RD   , 0xD8)
              , (WD   , 0xDC)
              , (TD   , 0xE0)
              , (STSW , 0xE8)
              ]

-- | Map Byte representation to Opcode
fromOp :: Byte -> Maybe Opcode
fromOp code = H.lookup code optbl
    where optbl :: HashMap Byte Opcode
          optbl = H.fromList
              [ (0x00, LDA  )
              , (0x04, LDX  )
              , (0x08, LDL  )
              , (0x0C, STA  )
              , (0x10, STX  )
              , (0x14, STL  )
              , (0x18, ADD  )
              , (0x1C, SUB  )
              , (0x20, MUL  )
              , (0x24, DIV  )
              , (0x28, COMP )
              , (0x2C, TIX  )
              , (0x30, JEQ  )
              , (0x34, JGT  )
              , (0x38, JLT  )
              , (0x3C, J    )
              , (0x40, AND  )
              , (0x44, OR   )
              , (0x48, JSUB )
              , (0x4C, RSUB )
              , (0x50, LDCH )
              , (0x54, STCH )
              , (0xD8, RD   )
              , (0xDC, WD   )
              , (0xE0, TD   )
              , (0xE8, STSW )
              ]


----------------------------------------------------------------------
-- lossy conversion between data types (loss of data with overflow)
--convert :: (Integral a, Integral b, Bits a, Bits b) => a -> b
--convert = fromBytes . toBytes

-- $memoryAccessors
--
-- Bytes in memory are assumed to be arranged in big endian
--
-- >>> let mem = fixedMemory 10
-- >>> getBytes 4 6 $ setWord 5 0x123456 mem
-- [0x0, 0x12, 0x34, 0x56, 0x0, 0x0]
-- >>> getWord 4 $ setBytes 5 [0xEF, 0x12, 0x34, 0x56, 0x78] mem
-- 0x123456
--
-- These accessors raise 'InvalidAddress' or 'InvalidAddressRange'
-- exception when the address is not valid within given memory.
--
-- >>> getByte 100 $ fixedMemory 10
-- InvalidAddress 100
-- >>> getWord 100 $ fixedMemory 10
-- InvalidAddressRange 100 102
--

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


----------------------------------------------------------------------
-- device IO

-- | @readByte dev@ reads a @byte@ to the @device@.
readByte :: Device -> IO Byte
readByte (Device h _) = B.head <$> B.hGet h 1

-- | @writeByte dev byte@ write a @byte@ to the @device@.
writeByte :: Device -> Byte -> IO ()
writeByte (Device _ h) b = B.hPut h $ B.singleton b


----------------------------------------------------------------------
-- Register access

-- $registerAccessors
--
-- We hide 'register' lens and control the access manually.
-- viewR, overR and setR are specific version of their original lens
-- accessors (without suffix R). They gets lens of type @'Lens''
-- 'Register' 'Word'@ and apply the @lens@ to resiter inside @st@.
-- These functions may perform validation works such as cutting
-- overflow bits.
--
-- >>> let st = cpuState (fixedMemory 10) emptyRegister []
-- >>> viewR a $ setR a 10 st
-- 10
-- >>> viewR a $ overR a (+5) st
-- 5
--

--viewR :: Getter Register Word -> m Word
-- | @viewR lens st@ acts just like @view lens reg@.
viewR :: MonadReader ST m => Getting Word Register Word -> m Word
-- | @overR lens f st@ acts just like @over lens f reg@.
-- @overR@ cuts overflow bits.
overR :: ASetter' Register Word -> (Word -> Word) -> ST -> ST
-- | @setR lens v st@ acts just like @set lens v reg@.
-- @setR@ cuts overflow bits.
setR :: ASetter' Register Word -> Word -> ST -> ST

viewR reg = view (register . reg)
overR reg f = over (register.reg) $ (0xFFFFFF .&.) . f
setR reg val = set (register.reg) $ 0xFFFFFF .&. val


----------------------------------------------------------------------
-- Constructors

-- | Fixed sized linear memory.
fixedMemory :: Int -> Memory
fixedMemory n = Memory $ V.replicate n 0

-- | Empty register with all values set to 0.
emptyRegister :: Register
emptyRegister = Register 0 0 0 0 0

-- | Create 'ST' with given memory, register, devices.
-- But in current version, given devices will be ignored and any steam
-- from/to device will be redirected to stdin or stdout.
cpuState :: Memory -> Register -> [(Byte,Device)]-> ST
cpuState mem reg ds = ST
    { _memory   = mem
    , _register = reg
    , _device   = const (Device stdin stdout)
    }
