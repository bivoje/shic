{-# LANGUAGE OverloadedStrings  #-}

module Assembler.Parser (assembly) where


import Data.Attoparsec.ByteString as P hiding (satisfy)
import Data.Attoparsec.ByteString.Char8 (
        satisfy, char, decimal, hexadecimal, endOfLine, isEndOfLine,
        skipSpace
    )

import Data.ByteString (ByteString)
import qualified Data.ByteString as B hiding (elem)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Control.Monad (void)

import Cpu.Types (Opcode)
import Assembler.Types

-- TODO use skipSpace in the palce of "\t"
-- TODO use scan on counting char (symbolName)

assembly :: Parser Assembly
assembly = do
    ignore
    (name, stAddr) <- option ("", 0) startLine
    ignore
    bls <- bodyLines
    ignore
    boot <- option (Value 0) endLine
    ignore
    endOfInput
    return $ Assembly name stAddr boot bls
    where ignore = many' commentLine

startLine :: Parser (ByteString, Int)
startLine = do
    name <- option "" symbolName
    "\tSTART"
    start <- option 0 ("\t" >> decimal)
    return (name, start)

bodyLines :: Parser [Line]
bodyLines = concat <$> (many1 line `sepBy` many1 commentLine)

endLine :: Parser Target
endLine = "\tEND" >> option (Value 0) ("\t" *> target)

line :: Parser Line
line = (actual <* clearLine) <?> "line"
    where actual = (Pragma <$> pragma) <|> command

-- comment line must start with . character
-- (no space allowed excepted the above line is also comment)
commentLine :: Parser ()
commentLine = ("." >> clearLine) <|> endOfLine

clearLine :: Parser ()
clearLine = takeTill isEndOfLine >> endOfLine

optional :: Parser a -> Parser ()
optional p = void p <|> return ()

pragma :: Parser Pragma
pragma = do
    label <- symbolName
    "\t"
    str <- P.takeWhile1 (/= 9) -- tab character
    "\t"
    case str of
        "BYTE" -> BYTE label <$> number
        "WORD" -> WORD label <$> number
        "RESB" -> RESB label <$> number
        "RESW" -> RESW label <$> number
        _ -> fail "directive"
  <?> "pragma"

command :: Parser Line
command = do
    label <- symbolName
    "\t"
    op <- opcode
    "\t"
    operand <- operand
    return $ Command label op operand
  <?> "command"

opcode :: Parser Opcode
opcode = do
    str <- B8.unpack <$> P.takeWhile (/= 9) -- tab character
    case reads str of
        [(op,"")] -> return op
        _ -> fail "opcode"

number :: Parser Int
number = decimal <|> hexNumber

hexNumber :: Parser Int
hexNumber = "X'" *> hexadecimal <* "'"

operand :: Parser Operand
operand = Operand <$> target <*> (optionBool ",X")

target :: Parser Target
target = (Value <$> decimal) <|> (Symbol <$> symbolName)

-- symbol name only consists of lowercase letters
-- less than or equal to 6
symbolName :: Parser ByteString
symbolName = do
    str <- P.takeWhile $ inClass "a-zA-Z"
    if B.length str <= 6 -- TODO this is inefficient
     then return str
     else fail "too long symbolName"

optionBool :: Parser a -> Parser Bool
optionBool p = option False $ const True <$> p

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing $ Just <$> p
