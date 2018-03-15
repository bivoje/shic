
module Assembler.TypesSpec where

import Test.Hspec
import Test.QuickCheck hiding (Result)

import qualified Data.ByteString as B
import Data.HashMap.Strict as H
import Control.Monad

import BasicTypes
import BasicTypesSpec
import Assembler.Types


spec :: Spec
spec = it "pass" $ 1 `shouldBe` 1


----------------------------------------------------------------------
-- * QuickCheck Generators

-- | Generates arbitrary 'Result' in ratio as they
-- likely appear in real object code.
-- Ignores @size@.
genResultLikely :: Gen Result
genResultLikely = frequency
    [ (9, ObjectCode <$> frequency
        [ (9, replicateM 3 arbitrary)
        , (1, (:[]) <$> arbitrary)
        ])
    , (1, Allocate <$> frequency
        [ (7, choose (1, 100))
        , (2, choose (101, 1000))
        , (1, choose (1001, 10000))
        ])
    ]

-- | Generates operand that is valid within given state.
genOperand :: ST -> Gen Operand
genOperand st = Operand <$> genTarget st <*> arbitrary

-- | Generates target that is valid within given state.
genTarget :: ST -> Gen Target
genTarget st =
    oneof [ Value . lowBits 15 <$> genWord
          , Symbol <$> elements (keys $_sym st)
          ]

instance Arbitrary ST where
    arbitrary =
        let genSymbol = symbolOf <$> arbitrary
            genSymtbl = fmap H.fromList . listOf1 $
                liftArbitrary2 genSymbol genWord
         in ST <$> genWord <*> genSymtbl


newtype GSymbol = GSymbol { symbolOf :: Symbol }
instance Arbitrary GSymbol where
    arbitrary = GSymbol . B.pack <$> (resize 6 . listOf1 $ genChar)
        where genChar = elements $ [0x61..0x7a] ++ [0x41..0x5a]
--                   ['a'..'z'] ++ ['A'..'Z']
