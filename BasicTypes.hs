{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BasicTypes
    ( Byte()
    , Word()
    , FWord()
    , Address()

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


type Byte = Word8

newtype Word = Word Word32 deriving (
    Read, Show, Eq, Num, Ord,
    Real, Enum, Integral, Bits,
    PrintfArg)

newtype FWord = FWord Word64 deriving (
    Read, Show, Eq, Num, Ord,
    Real, Enum, Integral, Bits)

type Address = Word


-- Extending Bits module

-- FIXME compose is too dangerous to use
-- silent overflow occurs when length bs * n < bitSize a

-- `compose n bs` takes lowest n bits from each of bs
-- then perform 2^n -ary composition (head is highest)
-- e.g. compose 8 [0x12, 0x34, 0x56] = 0x123456
-- `commpose n` is inverse of `decompose n`
-- Produces runtime error if n > bitSize a
compose :: (Bits b, Bits a) => Int -> [b] -> a
compose n = foldl (\a b -> (a `shiftL` n) .|. f b) zeroBits
    where f = lowBits n

-- abbriviation, since decomposeBottom is too long to use
dcpB :: (Bits a, Bits b) => Int -> Int -> a -> [b]
dcpB = decomposeBottom

-- returns exactly m elents from bottom
decomposeBottom :: (Bits a, Bits b) => Int -> Int -> a -> [b]
decomposeBottom m n =
    reverse . take m . (++ replicate m zeroBits) . decompose' n

decompose :: (Bits a, Bits b) => Int -> a -> [b]
decompose n = reverse . decompose n

-- `decompose n as` split bits of `a` by width n
-- then put them in list (head is lowest)
-- e.g. compose 8 0x123456 = [0x12, 0x34, 0x56] 
-- `decommpose n` is inverse of `compose n`
-- Produces runtime error if n > bitSize b
decompose' :: (Bits a, Bits b) => Int -> a -> [b]
decompose' n = unfoldr (\acc ->
    if acc == zeroBits then Nothing else Just
        (f acc, acc `shiftR` n)
    )
    where f = lowBits n

-- `lowBits n a` take lowest n bits from a
-- and returns in specified type wraped in Just
-- unless conversion loses bits in a
-- Produces runtime error if n > bitSize b
lowBitsMaybe :: (Bits a, Bits b) => Int -> a -> Maybe b
lowBitsMaybe n a
    | a `shiftR` n == zeroBits = Just $ lowBits n a
    | otherwise = Nothing

-- `lowBits n a` take lowest n bits from a
-- and returns in specified type
-- Produces runtime error if n > bitSize b
lowBits :: (Bits a, Bits b) => Int -> a -> b
lowBits n a =
    let zero = zeroBits 
        check = case bitSizeMaybe zero of
            Nothing -> True
            Just x  -> n <= x
        value = foldl f zero [0 .. n-1]
     in if check then value else undefined
    where f b i = if testBit a i then setBit b i else b

