
module AssemblerSpec where

import Prelude hiding (Word)

import Test.Hspec
import Test.QuickCheck hiding (Result)

import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.HashMap.Strict
import Control.Monad.State
import Control.Monad.Extra (iterateM)
import Control.Exception (evaluate)

import BasicTypes
import BasicTypesSpec hiding (spec)
import Assembler
import Assembler.Types
import Assembler.TypesSpec
import Loader.Parser
import Loader.Types (TextRecord(..), Object(..))
import Loader.TypesSpec
import Cpu.Types (Opcode(..))
import Cpu.TypesSpec


spec :: Spec
spec = do
  --describe "translateTarget" $ do
  --  context "when target address is out of range" $ do
  --    it "throws exception" $ do
  --      let target = Value 0x8000
  --      let io = testState $ translateTarget target
  --      io `shouldThrow` (== InvalidAddressing target)
  --  it "returns Address in range for static State" test_prop_trsTarget
--  describe "addLable" $ do
--    context "when duplicated key inserted" $ do
--      it "throws DuplicatedSymbol exception" $ do
--        let label = pack "LABEL"
--        let io = testState $ addLabel label >> addLabel label
--        io `shouldThrow` (== DuplicatedSymbol label)
  describe "translateCommand" $ do
    it "runs for static State" test_prop_trsCommand
  describe "collectText" $ do
    it "collects TextRecords being <30 bytes" test_prop_collectSize
  describe "dumpTextRecord" $ do
    it "is reverse of Loader.Parser.textrc" $
      property prop_dumpTextRecord_rev
  describe "dumpObject" $ do
    it "is reverse of Loader.Parser.object" $ do
      property prop_dumpObject_rev
  describe "assemble parser" $ do
    it "parse SIC assembly code" $ do
      let si = si' "sample4.asm" "sample4.obj"
      assembler `si` id

prop_dumpObject_rev :: Object -> Bool
prop_dumpObject_rev obj =
    let res = parseDone object (dumpObject obj)
     in P.compareResults res (P.Done B.empty obj) == Just True

prop_dumpTextRecord_rev :: TextRecord -> Bool
prop_dumpTextRecord_rev tr =
    let res = parseDone textrc (dumpTextRecord tr)
     in P.compareResults res (P.Done B.empty tr) == Just True

testState :: State ST a -> IO (a, ST)
testState stt = let (a,st) = runState stt $ ST 0 empty
                 in evaluate a >> evaluate st >> return (a,st)

--test_prop_trsTarget :: Property
--test_prop_trsTarget = forAll genPair $ uncurry prop_trsTarget
--    where genPair = genST >>= \st ->
--              liftArbitrary2 (return st) (genTarget st)

--prop_trsTarget :: ST -> Target -> Bool
--prop_trsTarget st tar =
--    let (word, st') = runState (translateTarget tar) st
--     in st == st' && check_address word
--
test_prop_trsCommand :: Property
test_prop_trsCommand =
    forAll genTri $ uncurry . uncurry $ prop_trsCommand
    where genTri :: Gen ((ST, Opcode), Operand)
          genTri = genST >>= \st -> liftArbitrary2
              (liftArbitrary2 (return st) arbitrary)
              (genOperand st)

prop_trsCommand :: ST -> Opcode -> Operand -> Bool
prop_trsCommand st opc opr =
    let (word, st') = runState (translateCommand opc opr) st
     in st == st'

test_prop_collectSize :: Property
test_prop_collectSize =
    forAll genPair $ uncurry prop_collectSize
    where genPair = liftArbitrary2
              (choose (0, 10000))
              (resize 500 (listOf1 arbitrary))

prop_collectSize :: Word -> [Result] -> Bool
prop_collectSize start rs =
    let itr = TextRecord start []
        trs = foldl collectText [itr] rs
     in all (\(TextRecord _ bs) -> length bs <= 30) trs


----------------------------------------------------------------------
-- * Pure Domain Checkers

check_ST :: ST -> Bool
check_ST (ST l s) = check_locctr l
    && all check_locctr (elems s)
    && all (not . B.null) (keys s)

check_symbol :: Symbol -> Bool
check_symbol sym = not (B.null sym) && B.all (flip elem charPool) sym
    where charPool = [0x61..0x7a] ++ [0x41..0x5a]
--                   ['a'..'z'] ++ ['A'..'Z']

check_locctr :: LOCCTR -> Bool
check_locctr = check_word


----------------------------------------------------------------------
-- * Helpers

-- shouldImplement'
--   :: Eq  a
--   => FilePath -> FilePath
--   -> (ByteString -> a) -> (ByteString -> a)
--   -> Excpectation
si' inf ouf f g = flip shouldReturn True $ do
    inc <- B.readFile ("test/" ++ inf)
    ouc <- B.readFile ("test/" ++ ouf)
    return $ f inc == g ouc
