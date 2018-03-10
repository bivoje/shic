{-# LANGUAGE OverloadedStrings  #-}

----------------------------------------------------------------------
-- |
-- Module      :  Assembler.Parser
--
-- Parser for SIC/XE assembly code (*.asm format)
-- implemented with @Data.Attoparsec.ByteString.Parser@s.
-- XE format is not supported yet.

module Assembler.Parser where

import Prelude hiding (Word)

import Data.Attoparsec.ByteString as P hiding (satisfy)
import Data.Attoparsec.ByteString.Char8 (
        satisfy, char, decimal, hexadecimal, endOfLine, isEndOfLine,
        skipSpace
    )

import Data.Bits (Bits)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B hiding (elem)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (isJust, fromMaybe)
import Control.Applicative ((<|>))
import Control.Monad (void, join)

import BasicTypes
import Assembler.Types
import Cpu.Types (Opcode)

-- TODO use skipSpace in the palce of "\t"
-- TODO use scan on counting char (symbolName)


----------------------------------------------------------------------
-- * Parser Helpers

-- | @checkMaybe f p@ tries to apply action p, and if the result
-- satisfies @f@ it returns the result wraped in @Just@. Otherwise
-- returns @Nothing@.
checkMaybe :: (a -> Bool) -> Parser a -> Parser (Maybe a)
checkMaybe f = fmap $ partial f
    where partial :: (a -> Bool) -> (a -> Maybe a)
          partial f a = if f a then Just a else Nothing
          -- replicates the one of `monadplus` package

-- | A combinator that enables complex validity check on a token parser.
check :: (a -> Bool) -> Parser a -> Parser a
check f p = check' f p "token matching constraint"

-- | Same as 'check', only that it enables specifying failure message .
check' :: (a -> Bool) -> Parser a -> String -> Parser a
check' f p dscrb = p >>= \a -> if f a then return a else fail dscrb

-- | Reimplementation of 'Text.Parsec.optionMaybe',
-- because @Data.Attoparsec.ByteString@ does not implement it.
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing $ Just <$> p

-- | @optionBool p@ tries to apply action p,
-- and returns whether it was successful.
optionBool :: Parser a -> Parser Bool
optionBool p = option False $ const True <$> p

-- | Match either a 'hexNumber' or a 'decimal'.
number :: (Integral a, Bits a) => Parser a
number = decimal <|> hexNumber

-- | Match a number in SIC/XE-hexadecimal format,
-- where preceeding @X'@ and following @'@ is neccessary
-- as in @X'07F3AD1'@.
hexNumber :: (Integral a, Bits a) => Parser a
hexNumber = "X'" *> hexadecimal <* "'"


----------------------------------------------------------------------
-- * Assembly Parsers

{- $syntax
    Following is sample SIC code to be used in explanation.

> CNT     START   256
>
> NUM     WORD    100        THE NUMBER TO EXAMINE
> MASK    WORD    1          MASK FOR BIT TESTING
> COUNT   WORD    0          ONE-WORD VARIABLE INITIALIZED TO 0
> ZERO    WORD    0          CONSTANT ZERO
> ONE     WORD    1          CONSTANT ONE
> DEV     BYTE    X'06'      OUTPUT DEVICE NUMBER
>
> main    J       func
>
> func    LDA     MASK       LOAD THE NUMBER TO `A`
>
> loopf   COMP    ZERO       COMPARE `MASK` WITH 0
>         JEQ     done       DONE IF `MASK` HAS OVERFLOWED
>         AND     NUM        A = mask & x
>         COMP    ZERO       COPARE THE BIT AT THE MASK
>         JEQ     noinc      SKIP INCREASING WHEN THE BIT IS 0
>         LDA     COUNT      LOAD COUNT
>         ADD     ONE        INCREASE COUNT BY ONE
>         STA     COUNT      SAVE TO COUNT
> noinc   LDA     MASK       LOAD THE MASK AGAIN
>         ADD     MASK       DOUBLE THE MASK
>         STA     MASK       SAVE MASK
>         J       loopf
>
> done    TD      DEV        TEST DEVICE
>         JEQ     done
>         LDCH    COUNT
>         WD      DEV        WRITE TO DEVICE
>
> halt    J       0
>         END     main
>
> .  int main()
> .  {
> .    int x = ??;
> .    int count = 0;
> .         int mask = 1;
> .         while(mask)
> .    {
> .      count += mask & x;
> .           mask += mask;
> .    }
> .  }
> .
> .  we need 3 variable x, count ,k
> .  but general registers are only 2 S, T
> .  A = k
> .  S = 0
> .  T = x COUNT = count

Every line in SIC assembly file which is not a comment consists of
tree parts, LABL, CODE and OPRN.

> CNT     START   256
> NUM     WORD    100        THE NUMBER TO EXAMINE \n
> ^^^^^   ^^^^^   ^^^^^      ^^^^^^^^^^^^^^^^^^^^^^
> LABL    CODE    OPRN       ignored

Each parts are seperated by tab character. Anything after the last
tab is ignored.

A line is an instruction if CODE is one of the 'OPCODE' or an pragma
if CODE is one of 'PRAGMA'. Interpreting LABL and OPRN is differed
depending on which type of line it is contained.

Program can start with @START@ pragma with a program name in place
of LABL and loading address in place of OPRN, where either of them
can be omitted. When omitted, program name and loading address
defaults to empty string and 0 respectively.

> CNT     START   256

@START@ pragma cannot appear in middle of code. It should either not
exist or the first non-comment line.

After @START@ pragma, any valid line can appear, e.i any pragma except
@START@ and @END@ or any instruction.

> NUM     WORD    100        THE NUMBER TO EXAMINE
> main    J       func

Interpretation of LABL and OPRN of pragma depends on the type of
pragma Pragmas @BYTE@, @WORD@, @RESB@, @RESW@ allocate space in
executable. So LABL holds the label for address (offset), which can
be omitted. And OPRN holds the size of allocation (if @RESW@ or
@RESB@) or the value (if @WORD@ or @BYTE@).

> NUM     WORD    100        ALLOCATE 3 BYTES WITH VALUE 0x000064
> RES     RESW    10         ALLOCATE 30 BYTES

For all 4 pragma LABL is parsed with 'symbolName' and OPRN is parse
with 'decimal'.

Interpretation of LABL and OPRN of instruction is fixed. LABL is
label (symbol) for the address of the instruction. OPRN is operand
for the instruction. You can omit label for any instruction.

Instruction is parsed with several parsers.

> func    LDA     COUNT,X    LOAD COUNT
>                 ^^^^^
>                   1
> ^^^^    ^^^     ^^^^^^^
>   2      3         4
> ^^^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^
>            5                    6
> ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
>                  7

1. 'target'
2. 'symbolName'
3. 'opcode'
4. 'operand'
5. 'command'
6. 'clearLine'
7. 'line'

Actually, if 1. ~ 5. is replaced with 'pragma', it becomse a parsing
sequence of pragma.

You can end program using following instruction. The assembler
generates objectcode @0x3C0000@ and the emulator would halt whenever
it sees @0x3C0000@.

> halt    J       0

@END@ pragma is used to finish the code. While LABL for @END@ pragma
is not allowed, OPRN is used to indicate boot address of the program.
Loader will set pc register of cpu to the OPRN address. You can use
symbol labels at here, so OPRN or @END@ pragma is parsed with 'target'.
@END@ pragma should be either omitted or the last non-comment line of the code. If OPRN or the whole pragma is omitted, boot address is set
to loading address.

>         END     main

A line starting with a single dot is considered comment, which is
parsed with 'commentLine'.

> .  int main()
> .  T = x COUNT = count

-}

-- | Parse the whole file
assembly :: Parser Assembly
assembly = do
    ignore
    (name, stAddr) <- option ("", 0) startLine
    ignore
    bls <- bodyLines
    ignore
    mboot <- join <$> optionMaybe endLine
    let boot = fromMaybe (Value stAddr) mboot
    ignore
    endOfInput
    return $ Assembly name stAddr boot bls
    where ignore = many' commentLine

----------------------------------------------------------------------
-- ** 3 types of line

-- | Fist of start-body-end trilogy.
-- Match the first line of assembly code, which contains "START".
-- The First line can optionally indicate name of the program and
-- address for the program to be loaded in memory.
startLine :: Parser (ByteString, Address)
startLine = do
    name <- option "" symbolName
    "\tSTART"
    start <- option 0 ("\t" >> address)
    return (name, start)

-- | Second of start-body-end trilogy.
-- Match all the lines between the first line and the last line,
-- which are instructions or pragmas or comments.
bodyLines :: Parser [Line]
bodyLines = concat <$> (many1 line `sepBy` many1 commentLine)

-- | Last of start-body-end trilogy.
-- Match the last line of assembly code, which contains "END".
-- The last line can optionally indicate the start address of
-- the program.
endLine :: Parser (Maybe Target)
endLine = "\tEND" >> optionMaybe ("\t" *> target)

-- | Match a line in assembly code and comsumes following characters
-- until end of line.
line :: Parser Line
line = (actual <* clearLine) <?> "line"
    where actual = (Pragma <$> pragma) <|> command

----------------------------------------------------------------------
-- ** Ignore Comments

-- | comment line must start with . character
-- (no space allowed excepted the above line is also comment)
commentLine :: Parser ()
commentLine = ("." >> clearLine) <|> endOfLine

-- | flush the line till the end of the line
clearLine :: Parser ()
clearLine = takeTill isEndOfLine >> endOfLine

----------------------------------------------------------------------
-- ** Parse Line

-- | Match an assembly pragma and its arguments.
pragma :: Parser Pragma
pragma = do
    label <- symbolName
    "\t"
    str <- P.takeWhile1 (/= 9) -- tab character
    "\t"
    case str of
        "BYTE" -> BYTE label <$> byte
        "WORD" -> WORD label <$> word
        "RESB" -> RESB label <$> number
        "RESW" -> RESW label <$> number
        _ -> fail "directive"
  <?> "pragma"

-- | Match an instruction, which consists of label, opcode and operand
-- separated by tab character.
command :: Parser Line
command = do
    label <- symbolName
    "\t"
    op <- opcode
    "\t"
    operand <- operand
    return $ Command label op operand
  <?> "command"

-- | Match an 'Opcode' which consists of upper case alphabets.
opcode :: Parser Opcode
opcode = do
    str <- B8.unpack <$> P.takeWhile (inClass "A-Z")
    case reads str of
        [(op,"")] -> return op
        _ -> fail "opcode"

-- | Match an operand of a instruction,
-- which is 'target' with a optional index specifier (",X").
operand :: Parser Operand
operand = Operand <$> target <*> (optionBool ",X")

-- | Match a target of a instruction,
-- which is 'address' or a 'symbolName'.
target :: Parser Target
target = (Value <$> address) <|> (Symbol <$> symbolName)

-- | Match a symbole name in SIC/XE.
-- Symbol name in SIC/XE consists 6 or less alphabets
-- (upper or lower). If given sequence of alphabets is too long,
-- parser fails with "too long symbol name".
symbolName :: Parser Symbol
symbolName =
    let p = P.takeWhile $ inClass "a-zA-Z"
    in check' (\bs -> B.length bs <= 6) p "too long symbol name"
-- FIXME, what if empty?

byte :: (Integral a, Bits a) => Parser a
byte = check' (<= 0xFF) number "too large byte"

word :: (Integral a, Bits a) => Parser a
word = check' (<= 0xFFFFFF) number "too large word"

address :: (Integral a, Bits a) => Parser a
address = check' (<= 0x7FFF) number "too large address"
