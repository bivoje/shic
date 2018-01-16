
module AssemblerSpec where

import Test.Hspec
--import Test.Hspec.Expectations

import Assembler (assembler)
import Data.ByteString (ByteString)
import Data.ByteString as B

main :: IO ()
main = hspec spec

-- shouldImplement'
--   :: Eq  a
--   => FilePath -> FilePath
--   -> (ByteString -> a) -> (ByteString -> a)
--   -> Excpectation
si' inf ouf f g = flip shouldReturn True $ do
    inc <- B.readFile ("test/" ++ inf)
    ouc <- B.readFile ("test/" ++ ouf)
    return $ f inc == g ouc

spec :: Spec
spec = do
  describe "assemble parser" $ do
    it "parse SIC assembly code" $ do
      let si = si' "sample4.asm" "sample4.obj"
      assembler `si` id
