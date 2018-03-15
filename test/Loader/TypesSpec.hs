
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
spec = it "pass" $ 1 `shouldBe` 1


----------------------------------------------------------------------
-- * QuickCheck Generators

-- | Generates arbitrary 'Object' in a form as they
-- likely appear in real object code.
genObjectLikely :: Gen Object
genObjectLikely = do
    (GSymbol name) <- arbitrary
    (GWord start') <- arbitrary
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

-- | Generates set of text records being mutually disjoint
-- to each other. The smallest starting address among that
-- of the resulting text records should be the given one.
-- Resulting text records might not be in order.
-- It ignores @size@.
--
-- TODO: make it size dependent
genTrs :: Address -> Gen [TextRecord]
genTrs addr = do
    -- generate subsequent list of TextRecords
    trs <- iterateM (\(TextRecord n bs) -> do
        jump <- genJump
        let start = n + lowBits 24 (length bs) + jump
        TextRecord start <$> listOf arbitrary
      ) =<< TextRecord addr <$> listOf arbitrary
    len <- choose (1, 1000)
    -- shuffle the list to gain more arbitrarity
    shuffle $ take len trs

-- | Generates jumps between text records in a object.
-- 90% of possibility that it does not jump.
genJump :: Gen Length
genJump = frequency [ (9, return 0), (1, choose (0, 1000)) ]

instance Arbitrary TextRecord where
    arbitrary = TextRecord <$> genWord <*> listOf arbitrary
