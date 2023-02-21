{-# LANGUAGE OverloadedStrings #-}

module Loader.Parser
    (
    -- * Parsers
      object
    , textrc
    , word
    , byte

    -- * Helpers
    , hexadecimal'
    , parseHandler
    , parseDone
    ) where

import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 (char, endOfLine)
import Data.Bits
import Data.List (foldl')
import Data.Char (isSpace)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Exception (throw)

import Loader.Types


-- | @parseHandler p content@ runs @p@ over @content@. If @p@ fails,
-- it throws 'ParseError' exception with the line number where
-- the parser failed.
parseHandler :: Parser a -> ByteString -> a
parseHandler p contents = case parseDone p contents of
    Done "" obj    -> obj
 -- Done _ _       -> not possible because of endOFInput
 -- Partial _      -> not possible because of feed
    Fail "" _ _    -> throw $ ParseError 0 "Debuging Required"
    Fail left ps p ->
        let totalLineno = length $ B8.lines contents
            unparsed = B8.lines left
            lineAfterError = "... " `B.append` head unparsed
            unparsedLineno = totalLineno - length unparsed + 1
        in throw $ ParseError unparsedLineno lineAfterError


-- | @parseDone p str@ evaluates parser @p@ on @str@ as it is whole.
-- It will never return 'Partial' result.
parseDone :: Parser a -> ByteString -> IResult ByteString a
parseDone p c = feed (parse p c) B.empty


-- | Match SIC object file format.
object :: Parser Object
object = do
    name  <- "H" *> fmap strip (P.take 6)
    start <- word
    len   <- word <* endOfLine
    body  <- textrc `sepBy` endOfLine
    endOfLine
    boot  <- "E" *> word
    option () endOfLine *> endOfInput
    return $ Object name start boot len body
  <?> "newline mismatch"
    where strip = B8.takeWhile (not.isSpace)

-- | Match a line of text records in SIC object format.
textrc :: Parser TextRecord
textrc = do
    char 'T'
    start <- word -- Address
    len <- byte -- Int
    objs <- count len byte <?> show len ++ " bytes in text"-- Word8
           -- ensures size of textrecord
    return $ TextRecord start objs

-- | Match hexrepresentation of a word which is in 6 characters long.
--
-- @
-- word = hexadecimal' 6
-- @
word :: (Integral a, Bits a) => Parser a
word = hexadecimal' 6

-- | Match hexrepresentation of a byte which is in 2 characters long.
--
-- @
-- byte = hexadecimal' 2
-- @
byte :: (Integral a, Bits a) => Parser a
byte = hexadecimal' 2

-- | Modification of hexadecimal in Data.Attoparsec.ByteString.Char8.
-- @hexadecimal' n@ accepts exactly @n@ characters from stream.
hexadecimal' :: (Integral a, Bits a) => Int -> Parser a
hexadecimal' n =
    let p = count n (satisfy isHexDigit)
                <?> show n ++ " hex characters"
    in fmap (foldl' step 0) p
    where
        isHexDigit w = (w >= 48 && w <= 57)  || -- 0-9
                       (w >= 97 && w <= 102) || -- a-f
                       (w >= 65 && w <= 70)     -- A-F
        step a w | w >= 48 && w <= 57  =
                        (a `shiftL` 4) .|. fromIntegral (w - 48)
                 | w >= 97             =
                        (a `shiftL` 4) .|. fromIntegral (w - 87)
                 | otherwise           =
                        (a `shiftL` 4) .|. fromIntegral (w - 55)
