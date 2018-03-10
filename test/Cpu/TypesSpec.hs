
module Cpu.TypesSpec where

import Test.Hspec
import Test.QuickCheck

import Cpu.Types


spec :: Spec
spec = do
  describe "Nothing" $ do
    it "goes well" $ do
      1 `shouldBe` 1

instance Arbitrary Opcode where
    arbitrary = elements
        [ LDA
        , LDX
        , LDL
        , STA
        , STX
        , STL
        , ADD
        , SUB
        , MUL
        , DIV
        , COMP
        , TIX
        , JEQ
        , JGT
        , JLT
        , J
        , AND
        , OR
        , JSUB
        , RSUB
        , LDCH
        , STCH
        , RD
        , WD
        , TD
        , STSW
        ]
