
module Loader.TypesSpec where

import Test.Hspec
import Test.QuickCheck hiding (Result)

import Data.Semigroup (Max(..))
import Control.Monad.Extra (iterateM)

import BasicTypes
import BasicTypesSpec
import Assembler.TypesSpec
import Loader.Types


spec :: Spec
spec = do
  describe "Nothing" $ do
    it "goes well" $ do
      1 `shouldBe` 1

instance Arbitrary Object where
    arbitrary = do
        name  <- genSymbol
        start' <- genWord
        jumpL <- genJump
        let start = min 0xFFFFFF $ jumpL + start'
        body <- genTrs start
        let (Max end') = mconcat $ map (\(TextRecord s bs) ->
                Max $ s + lowBits 24 (length bs)) body
        jumpR <- genJump
        let end = min 0xFFFFFF $ jumpR + end'
        let len = end - start
        boot <- choose (start, end)
        return $ Object name start boot len body

instance Arbitrary TextRecord where
    arbitrary = TextRecord <$> genWord <*> listOf arbitrary

genTrs :: Address -> Gen [TextRecord]
genTrs addr = do
    -- generate subsequent list of TextRecords
    trs <- iterateM (\(TextRecord n bs) -> do
        jump <- genJump
        let start = n + lowBits 24 (length bs) + jump
        TextRecord start <$> listOf arbitrary
      ) =<< TextRecord addr <$> listOf arbitrary
    len <- choose (1, min 1000 $ 0xFFFFFF - lowBits 24 addr)
    -- shuffle the list to gain more arbitrarity
    shuffle $ take len trs

genJump :: Gen Length
genJump = frequency [ (9, return 0), (1, choose (0, 1000)) ]
