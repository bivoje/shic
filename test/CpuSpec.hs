
module CpuSpec where

import Test.Hspec
--import Test.Hspec.Expectations

import Cpu
import Cpu.Types
import Loader (loader)
import Data.ByteString (ByteString)
import Data.ByteString as B
import Data.Vector as V

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "assemble parser" $ do
    it "parse SIC assembly code" $ do
      comp `shouldReturn` True

comp :: IO Bool
comp = do
    gogo <- go
    smsm <- sample
    return $ gogo == smsm

go :: IO (Memory, Register)
go = do
    obj <- B.readFile "test/sample4.obj"
    let clean_mem = fixedMemory 350
    let (mem, pcv) = loader obj clean_mem
    let st = setR pc pcv $ cpuState mem emptyRegister []
    (ST mem reg _) <- runCpu st
    return (mem, reg)

sample :: IO (Memory, Register)
sample = do
    obj <- B.readFile "test/memory4.dump"
    let clean_mem = fixedMemory 350
    let (mem, _) = loader obj clean_mem
    let reg = Register {_a = 0, _x = 0, _l = 0, _pc = 329, _sw = 128}
    return (mem, reg)
