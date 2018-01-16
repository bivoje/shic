{-# LANGUAGE OverloadedStrings #-}

module Assembler (runAssembler) where

import Prelude hiding (Word)

import System.IO
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Bits
import Data.Monoid
import Data.List (intersperse, dropWhileEnd)
import Data.Maybe (fromJust, isNothing)
import Control.Exception
import Control.Monad.State
import Control.Lens

import Data.Attoparsec.ByteString (parseOnly)
import Numeric (showHex)

import BasicTypes
import Cpu.Types (Opcode(..))
import Loader.Types
import Loader.Parser (parseHandler)
import Assembler.Types
import Assembler.Parser


optbl :: HashMap Opcode Byte
optbl = H.fromList
    [ (LDA  , 0x00)
    , (LDX  , 0x04)
    , (LDL  , 0x08)
    , (STA  , 0x0C)
    , (STX  , 0x10)
    , (STL  , 0x14)
    , (ADD  , 0x18)
    , (SUB  , 0x1C)
    , (MUL  , 0x20)
    , (DIV  , 0x24)
    , (COMP , 0x28)
    , (TIX  , 0x2C)
    , (JEQ  , 0x30)
    , (JGT  , 0x34)
    , (JLT  , 0x38)
    , (J    , 0x3C)
    , (AND  , 0x40)
    , (OR   , 0x44)
    , (JSUB , 0x48)
    , (RSUB , 0x4C)
    , (LDCH , 0x50)
    , (STCH , 0x54)
    , (RD   , 0xD8)
    , (WD   , 0xDC)
    , (TD   , 0xE0)
    , (STSW , 0xE8)
    ]

getOp :: Opcode -> Byte
getOp op = fromJust $ H.lookup op optbl


runAssembler :: FilePath -> IO ()
runAssembler filepath = do
    contents <- B.readFile filepath
    let outfile = dropWhileEnd (/='.') filepath ++ "obj"
    let ls = parseHandler assembly contents
    let objxt = dumpObject . assemble $ ls
    B.writeFile outfile objxt


-- Dump

-- dump an object to bytestring object file representation
dumpObject :: Object -> ByteString
dumpObject (Object name' start boot len trs) =
    let name   = B.take 6 $ name' `B.append` "      "
        header = ["H", name, formatInt 6 start, formatInt 6 len, "\n"]
        body   = intersperse "\n" . map dumpTextRecord $ trs
        footer = ["\n", "E", formatInt 6 boot, "\n"]
    in B.concat $ header ++ body ++ footer

-- dump textrecord to bytestring
dumpTextRecord  :: TextRecord -> ByteString
dumpTextRecord (TextRecord st bs)
    | length bs > 30 = undefined -- invalid, fault of collectText
dumpTextRecord (TextRecord st bs) = B.concat $
    [ "T"
    , formatInt 6 st
    , formatInt 2 (length bs)
    ] ++ map (formatInt 2) bs

-- format Int to fixed width (n) hex representation
formatInt :: Bits a => Int -> a -> ByteString
formatInt n = B.pack . map step . dcpB n 4
    where step w | 0  <= w && w < 10 = w + 48
                 | 10 <= w && w < 16 = w + 55
                 | otherwise = undefined


-- Assembler

-- assemble through one and two pass
assemble :: Assembly -> Object
assemble (Assembly name start' boot' ls) =
    let start = conv start'
        ta = evalState (resolveTarget boot') st
        boot = conv ta
        st@(ST size symtbl) = onePass start' ls
        ts = reverse $ twoPass start' symtbl ls
     in Object name start boot (size - start') ts
     where conv x = case lowBitsMaybe 24 x of
            Just x -> x
            Nothing -> throw ProgramTooBig

-- what should size be?


-- Two Pass

-- returns textrecord in reversed order
twoPass :: LOCCTR -> SymTbl -> [Line] -> [TextRecord]
twoPass start symtbl ls =
    let rs = evalState (forM ls twoStep) (ST start symtbl)
     in foldl collectText [TextRecord (lowBits 24 start) []] rs

-- collect Result to list of text records
collectText :: [TextRecord] -> Result -> [TextRecord]
-- collect text should be called with at least one (empty) TextRecord
collectText [] _ = undefined
-- `object code` will be concat'ed to textrecord
collectText tts@((TextRecord st ws):ts) (ObjectCode bs)
    | length (bs ++ ws) <= 30 = TextRecord st (ws ++ bs) : ts
    | otherwise = let start = fromIntegral st + length ws
                  in case lowBitsMaybe 24 start of
                      Just start' -> TextRecord start' bs : tts
                      Nothing -> throw ProgramTooBig
-- `allocate` will break current and append new text record
collectText tts                         (Allocate 0) = tts
collectText tts@((TextRecord st ws):ts) (Allocate n) =
    let start = fromIntegral st + length ws + n
        appendTo = if null ws then ts else tts
    in case lowBitsMaybe 24 start of
        Just start' -> TextRecord start' [] : appendTo
        Nothing -> throw ProgramTooBig

-- translate a line
twoStep :: Line -> State ST Result
twoStep line = do
    r <- go line
    modify $ over loc (+ locinc r)
    return r
    where go (Command _ opc op) = translateCommand opc op
          go (Pragma pragma) = return $ translatePragma pragma
          locinc (ObjectCode bs) = length bs
          locinc (Allocate l) = l

-- translate pragma
translatePragma :: Pragma -> Result
translatePragma (BYTE _ b  ) = ObjectCode $ dcpB 1 8 b
translatePragma (WORD _ b  ) = ObjectCode $ dcpB 3 8 b
translatePragma (RESB _ len) = allocate len
translatePragma (RESW _ len) = allocate (len*3)

-- error prone allocation
allocate :: Int -> Result
allocate len | len < 0 = throw $ InvalidReservation len
             | otherwise = Allocate len

-- translate command
translateCommand :: Opcode -> Operand -> State ST Result
translateCommand  op (Operand tar x) =
    let code = (`shiftL` 16) . lowBits 8 $ getOp op
        sx = if x then (`setBit` 15) else id
     in do ta <- translateTA tar
           return . ObjectCode . dcpB 3 8 . sx $ code .|. ta

-- translate Target to address
translateTA :: Target -> State ST Word
translateTA tar = do
    ta <- resolveTarget tar
    case lowBitsMaybe 15 ta of
        Just x  -> return x
        Nothing -> throw $ InvalidAddressing tar

resolveTarget :: Target -> State ST Int
resolveTarget (Symbol symbol) = resolveSymbol symbol
resolveTarget (Value val) = return val

-- resolve address from symbol
resolveSymbol :: ByteString -> State ST Int
resolveSymbol symbol = do
    symtbl <- gets _sym
    return $ case H.lookup symbol symtbl of
        Just x -> x
        Nothing -> throw $ UnknownSymbol symbol


-- One Pass

-- run one pass assembler
onePass :: LOCCTR -> [Line] -> ST
onePass start ls = execState (forM_ ls oneStep) (ST start H.empty)

-- handle a line of assembly
oneStep :: Line -> State ST ()
oneStep (Command label _ _) =
    addLabel label >> (modify $ over loc (+3))
oneStep (Pragma (BYTE label _)) =
    addLabel label >> (modify $ over loc (+1))
oneStep (Pragma (WORD label _)) =
    addLabel label >> (modify $ over loc (+3))
oneStep (Pragma (RESB label len)) =
    addLabel label >> (modify $ over loc (+len))
oneStep (Pragma (RESW label len)) =
    addLabel label >> (modify $ over loc (+(3*len)))

-- add label to SymTbl unless already seen
addLabel :: ByteString -> State ST ()
addLabel "" = return ()
addLabel label = do
    locctr <- gets _loc
    modify $ over sym (H.insertWith raise label locctr)
    where raise _ _ = throw $ DuplicatedSymbol label

