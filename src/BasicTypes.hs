{-# LANGUAGE GeneralizedNewtypeDeriving #-}

----------------------------------------------------------------------
-- |
-- Module      :  BasicTypes
--
-- Basic data types and modifiers used in library.

module BasicTypes
    (
    -- * Data Types
    -- $datatypes
      Byte()
    , Word()
    , FWord()
    , Address()

    -- * Data accessors
    -- ** Extending Bits module
    , compose
    , dcpB
    , decomposeBottom
    , decompose
    , decompose'
    , lowBitsMaybe
    , lowBits
    ) where


import Prelude hiding (Word)

import Data.Word (Word8, Word32, Word64)
import Data.Bits
import Text.Printf (PrintfArg)

import Data.List (unfoldr)

-- $datatypes
--
-- These type signitures are considered bad habits.
-- They will be altered soon.

-- | A byte in SIC/XE is 8 bits long.
-- 'Byte'@ is defined with a compatible data type.
type Byte = Word8

-- | A word in SIC/XE is 24 bits long.
-- 'Word' is defined with a compatible data type.
newtype Word = Word Word32 deriving (
    Read, Show, Eq, Num, Ord,
    Real, Enum, Integral, Bits,
    PrintfArg)

-- | Floating point number is represented with 48 bits in SIC/XE.
-- 'FWord' is defined with a compatible data type.
newtype FWord = FWord Word64 deriving (
    Read, Show, Eq, Num, Ord,
    Real, Enum, Integral, Bits)

-- | SIC/XE is capable of 2^15 bytes of memory.
-- 'Address' is defined with a compatible type.
type Address = Word


----------------------------------------------------------------------
-- Extending Bits module

-- FIXME compose is too dangerous to use
-- silent overflow occurs when length bs * n < bitSize a

-- | @compose n bs@ takes lowest @n@ bits from each of bs,
-- then performs / 2^n -ary / composition (head is highest).
-- @'commpose' n@ is inverse of @'decompose' n@.
--
-- e.g.
--
-- @
-- compose 8 [0x12, 0x34, 0x56] == 0x123456
-- @
--
-- It produces runtime error if @n > bitSize a@.
compose :: (Bits b, Bits a) => Int -> [b] -> a
compose n = foldl (\a b -> (a `shiftL` n) .|. f b) zeroBits
    where f = lowBits n

-- | abbriviation of 'decomposeBottom',
-- since decomposeBottom is too long to use.
dcpB :: (Bits a, Bits b) => Int -> Int -> a -> [b]
dcpB = decomposeBottom

-- | @decomposeBottom m n bs@ divides @bs@ into chunks of @n@ bits
-- and returns exactly lowest @m@ chunks among those.
--
-- e.g.
--
-- @
-- decoposeBottom 3 8 0x12345678 == [0x34, 0x56, 0x78]
-- decoposeBottom 3 8 0x00000123 == [0x00, 0x01, 0x23]
-- @
decomposeBottom :: (Bits a, Bits b) => Int -> Int -> a -> [b]
decomposeBottom m n =
    reverse . take m . (++ replicate m zeroBits) . decompose' n

-- | @decompose n as@ splits bits of @a@ by width @n@
-- then put them in list (head is lowest).
-- @'commpose' n@ is inverse of @'decompose' n@.
--
-- e.g.
--
-- @
-- compose 8 0x123456 == [0x12, 0x34, 0x56]
-- @
--
-- It produces runtime error if @n > bitSize b@.
decompose :: (Bits a, Bits b) => Int -> a -> [b]
decompose n = reverse . decompose' n

-- | @decompose' n a@ is reverse of @decompose n a@.
--
-- @
-- decompose' n == reverse . decompose n
-- @
decompose' :: (Bits a, Bits b) => Int -> a -> [b]
decompose' n = unfoldr (\acc ->
    if acc == zeroBits then Nothing else Just
        (f acc, acc `shiftR` n)
    )
    where f = lowBits n

-- | @lowBits n a@ take lowest @n@ bits from @a@
-- and returns in specified type wraped in Just
-- unless conversion loses valid bits from @a@.
--
-- It produces runtime error if @n > bitSize b@
lowBitsMaybe :: (Bits a, Bits b) => Int -> a -> Maybe b
lowBitsMaybe n a
    | a `shiftR` n == zeroBits = Just $ lowBits n a
    | otherwise = Nothing

-- | @lowBits n a@ take lowest @n@ bits from @a@
-- and returns in specified type.
--
-- It produces runtime error if @n > bitSize b@.
lowBits :: (Bits a, Bits b) => Int -> a -> b
lowBits n a =
    let zero = zeroBits
        check = case bitSizeMaybe zero of
            Nothing -> True
            Just x  -> n <= x
        value = foldl f zero [0 .. n-1]
     in if check then value else undefined
    where f b i = if testBit a i then setBit b i else b

