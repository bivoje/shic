{-# LANGUAGE OverloadedStrings #-}

module Assembler
    (
    -- * Assembler
      assembler
    , assemble

    -- ** One Pass
    , onePass
    , oneStep
    , addLabel

    -- ** Two Pass
    , twoPass
    , collectText
    , twoStep
    , translatePragma
    , translateCommand
    , resolveTarget
    , resolveSymbol

    -- * Dumping
    , dumpObject
    , dumpTextRecord
    , formatInt

    ) where


import Prelude hiding (Word)

import System.IO
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import Data.Bits
import Data.Monoid
import Data.List (intersperse, dropWhileEnd)
import Data.Maybe (isNothing)
import Control.Exception
import Control.Monad.State
import Control.Lens

import Data.Attoparsec.ByteString (parseOnly)
import Numeric (showHex)

import BasicTypes
import Cpu.Types (Opcode(..), getOp)
import Loader.Types
import Loader.Parser (parseHandler)
import Assembler.Types
import Assembler.Parser as AP


----------------------------------------------------------------------
-- * Assembler

-- | 2-Pass assembler.
-- Gets SIC source code and returns SIC object code
assembler :: ByteString -> ByteString
assembler contents =
    let ls = parseHandler assembly contents
     in dumpObject $ assemble ls

-- | Perform 2-Pass SIC assemble task. If the size of executable is
-- too big, @assemble@ throws ProgramTooBig exception.
assemble :: Assembly -> Object
assemble (Assembly name start' boot' ls) =
    let st@(ST size symtbl) = onePass start' ls
        start = conv start'
        boot = conv $ evalState (resolveTarget boot') st
        ts = reverse $ twoPass start' symtbl ls
     in Object name start boot (lowBits 24 $ size - start') ts
     where conv x = case lowBitsMaybe 24 x of
            Just x -> x
            Nothing -> throw ProgramTooBig


----------------------------------------------------------------------
-- * Dumping

-- | Dump an 'Object' to bytestring object file representation.
dumpObject :: Object -> ByteString
dumpObject (Object name' start boot len trs) =
    let name   = B.take 6 $ name' `B.append` "      "
        header = ["H", name, formatInt 6 start, formatInt 6 len, "\n"]
        body   = intersperse "\n" . map dumpTextRecord $ trs
        footer = ["\n", "E", formatInt 6 boot, "\n"]
    in B.concat $ header ++ body ++ footer

-- | Dump 'TextRecord' to bytestring.
dumpTextRecord  :: TextRecord -> ByteString
dumpTextRecord (TextRecord st bs) = B.concat $
    [ "T"
    , formatInt 6 st
    , formatInt 2 (length bs)
    ] ++ map (formatInt 2) bs

-- | Format Int to fixed width (n) hex representation.
--
-- @
-- formatInt 6 0x1234 == "001234"
-- @
formatInt :: Bits a => Int -> a -> ByteString
formatInt n = B.pack . map step . dcpB n 4
    where step w | 0  <= w && w < 10 = w + 48
                 | 10 <= w && w < 16 = w + 55
                 | otherwise = undefined


----------------------------------------------------------------------
-- ** Two Pass

-- | @twoPass startAddr symtbl ls@ returns collection of textrecord in
-- reversed order. @symtbl@ should idealy be from 'onePass'.
twoPass :: LOCCTR -> SymTbl -> [Line] -> [TextRecord]
twoPass start symtbl ls =
    let rs = evalState (forM ls twoStep) (ST start symtbl)
     in foldl collectText [TextRecord (lowBits 24 start) []] rs

-- | @collectText ts r@ appends @r@ to @ts@. Contextually,
-- @foldl collectText _ rs@ will concat objectcodes in rs, except some
-- cases. It splits text record when 1) allocation occurs or 2) textrecord
-- is too long. @collectText@ ignores allocation of 0 length so that
-- @Allocate 0@ is used to manually split @TextRecord@ in two-step
-- process.
collectText :: [TextRecord] -> Result -> [TextRecord]
-- collect text should be called with at least one (empty) TextRecord
collectText [] _ = undefined
-- @object code@ will be concat'ed to textrecord
collectText tts@((TextRecord st ws):ts) (ObjectCode bs)
    | length (bs ++ ws) <= 30 = TextRecord st (ws ++ bs) : ts
    | otherwise = let start = fromIntegral st + length ws
                  in case lowBitsMaybe 24 start of
                      Just start' -> TextRecord start' bs : tts
                      Nothing -> throw ProgramTooBig
-- 'Allocate' will break current and append new text record
collectText tts                         (Allocate 0) = tts
collectText tts@((TextRecord st ws):ts) (Allocate n) =
    let start = st + lowBits 24 (length ws) + n
        appendTo = if null ws then ts else tts
    in case lowBitsMaybe 24 start of
        Just start' -> TextRecord start' [] : appendTo
        Nothing -> throw ProgramTooBig

-- | Handles each line during two-pass. Translate and increase LOCCTR.
twoStep :: Line -> State ST Result
twoStep line = do
    r <- trans line
    modify $ over loc (locinc r)
    return r
    where trans (Command _ opc op) = translateCommand opc op
          trans (Pragma pragma) = return $ translatePragma pragma
          locinc r lc = let inc = width r + lowBits 24 lc
              in case lowBitsMaybe 24 inc of
                  Just nlc -> nlc
                  Nothing -> throw $ AddressTooLarge inc
          width (ObjectCode bs) = lowBits 24 $ length bs
          width (Allocate l) = l

-- | @translatePragma pragma@ generates bytes or allocates space in
-- executable depending on type of @pragma@.
translatePragma :: Pragma -> Result
translatePragma (BYTE _ b  ) = ObjectCode $ dcpB 1 8 b
translatePragma (WORD _ b  ) = ObjectCode $ dcpB 3 8 b
translatePragma (RESB _ len) = allocate len
translatePragma (RESW _ len) = allocate (len*3)

-- error prone allocation
allocate :: Length -> Result
allocate len | len < 0 = throw $ InvalidReservation len
             | otherwise = Allocate len

-- | @translateCommand opcode oprn@ constructs object code for
-- instruction @label opcode oprn@ then split it into bytes.
-- @translateCommand@ does not modify state, only reads it.
translateCommand :: Opcode -> Operand -> State ST Result
translateCommand  op (Operand tar x) =
    let code = (`shiftL` 16) . lowBits 8 $ getOp op
        sx = if x then (`setBit` 15) else id
     in do ta <- resolveTarget tar
           return . ObjectCode . dcpB 3 8 . sx $ code .|. ta

-- | @resolveTarget target@ resolve address from @target@.
-- @resolveTarget@ does not modify state, only reads it.
resolveTarget :: Target -> State ST Address
resolveTarget (Symbol symbol) = resolveSymbol symbol
resolveTarget (Value val) = return $ lowBits 24 val

-- | @resolveSymbol label@ resolves address from @label@. It throws
-- 'UnknownSymbol' exception when @label@ does not apear in symbol
-- table. @resolvesymbol@ does not modify state, only reads it.
resolveSymbol :: Symbol -> State ST Address
resolveSymbol symbol = do
    symtbl <- gets _sym
    return $ case H.lookup symbol symtbl of
        Just x -> x
        Nothing -> throw $ UnknownSymbol symbol


-----------------------------------------------------------------------
-- ** One Pass

-- | @onePass loc ls@ runs one-pass assembler through ls,
-- produces size of the excutable and symbol table.
onePass :: LOCCTR -> [Line] -> ST
onePass start ls = execState (forM_ ls oneStep) (ST start H.empty)

-- | @oneStep line@ handle a line of assembly.
-- It adds label to symbol table and increase LOCCTR for each line.
oneStep :: Line -> State ST ()
oneStep (Command label _ _) =
    addLabel label >> (modify $ over loc (+3))
oneStep (Pragma (BYTE label _)) =
    addLabel label >> (modify $ over loc (+1))
oneStep (Pragma (WORD label _)) =
    addLabel label >> (modify $ over loc (+3))
oneStep (Pragma (RESB label len)) =
    addLabel label >> (modify $ over loc (+ lowBits 24 len))
oneStep (Pragma (RESW label len)) =
    addLabel label >> (modify $ over loc (+ 3* lowBits 24 len))

-- | Add label to 'SymTbl' unless the label has already been seen,
-- in which case @addLable@ throws 'DuplicatedSymbol' exception.
addLabel :: Symbol -> State ST ()
addLabel "" = return ()
addLabel label = do
    locctr <- gets _loc
    modify $ over sym (H.insertWith raise label locctr)
    where raise _ _ = throw $ DuplicatedSymbol label

