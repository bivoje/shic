
module BasicTypesSpec where

import Prelude hiding (Word)
import Test.Hspec
import Test.QuickCheck

import BasicTypes


spec :: Spec
spec = it "pass" $ 1 `shouldBe` 1


----------------------------------------------------------------------
-- * Validity Checkers

-- $validityCheckers
-- checks if variable of basic type is in valid range.

check_byte :: Byte -> Bool
check_byte byte = byte <= 0xFF

check_word :: Word -> Bool
check_word word = word <= 0xFFFFFF

check_fword :: FWord -> Bool
check_fword fword = fword <= 0xFFFFFFFFFFFF

check_address :: Address -> Bool
check_address address = address <= 0x7FFF


----------------------------------------------------------------------
-- * Generators

-- $generators
-- basic type generators

newtype GWord = GWord { wordOf :: Word }
instance Arbitrary GWord where
    arbitrary = GWord <$> choose (0, 0xFFFFFF)

genWord :: Gen Word
genWord = wordOf <$> arbitrary

newtype GAddress = GAddress { addrOf :: Address }
instance Arbitrary GAddress where
    arbitrary = GAddress <$> choose (0, 0xFFFFFF)

genAddress :: Gen Address
genAddress = addrOf <$> arbitrary
