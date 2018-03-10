
module Assembler.TypesSpec where

import Test.Hspec
import Test.QuickCheck hiding (Result)

import qualified Data.ByteString as B
import Data.HashMap.Strict as H

import BasicTypes
import BasicTypesSpec
import Assembler.Types

spec :: Spec
spec = do
  describe "Nothing" $ do
    it "goes well" $ do
      1 `shouldBe` 1

----------------------------------------------------------------------
-- * QuickCheck Generators

instance Arbitrary Result where
    arbitrary = frequency
        [ (9, ObjectCode <$> frequency
            [ (9, sequence $ replicate 3 arbitrary)
            , (1, (:[]) <$> arbitrary)
            ])
        , (1, Allocate <$> frequency
            [ (7, choose (1, 100))
            , (2, choose (101, 1000))
            , (1, choose (1001, 10000))
            ])
        ]
    -- use frequency to properly simulate Result
    -- consecutive of Allocations, LARGE allocation,
    -- consecutive bytecodes, etc...
    --  // instructions are mapped to bytecodes
    --  // allocations are sometimes mapped to bytecodes. hmm.

genST :: Gen ST
genST = let genSymtbl = fmap H.fromList . listOf1 $
                liftArbitrary2 genSymbol genWord
         in ST <$> genWord <*> genSymtbl

genTarget :: ST -> Gen Target
genTarget st =
    oneof [ Value . lowBits 15 <$> genWord
          , Symbol <$> elements (keys $_sym st)
          ]

genOperand :: ST -> Gen Operand
genOperand st = Operand <$> genTarget st <*> arbitrary

genSymbol :: Gen Symbol
genSymbol = fmap B.pack . resize 6 . listOf1 . elements $ charPool
    where charPool = [0x61..0x7a] ++ [0x41..0x5a]
--                   ['a'..'z'] ++ ['A'..'Z']
