
module BasicTypesSpec where

import Prelude hiding (Word)
import Test.Hspec
import Test.QuickCheck

import BasicTypes


spec :: Spec
spec = do
  describe "Nothing" $ do
    it "goes well" $ do
      1 `shouldBe` 1

check_byte :: Byte -> Bool
check_byte byte = byte <= 0xFF

check_word :: Word -> Bool
check_word word = word <= 0xFFFFFF

check_fword :: FWord -> Bool
check_fword fword = fword <= 0xFFFFFFFFFFFF

check_address :: Address -> Bool
check_address address = address <= 0x7FFF

genWord :: Gen Word
genWord = choose (0, 0xFFFFFF)

newtype GAddress = GAddress Address
instance Arbitrary GAddress where
    arbitrary = GAddress <$> choose (0, 0xFFFFFF)

genAddress :: Gen Address
genAddress = do
    GAddress addr <- arbitrary
    return addr
