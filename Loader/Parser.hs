{-# LANGUAGE OverloadedStrings #-}
module Loader.Parser (object, parseHandler) where


import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 (char, endOfLine)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B

import Control.Exception (throw)
import Data.List (foldl')

import Loader.Types


parseHandler :: Parser a -> ByteString -> a
parseHandler p contents = case parseDone p contents of
    Done "" obj    -> obj
 -- Done _ _       -> not possible because of endOFInput
 -- Partial _      -> not possible because of feed
    Fail "" _ _    -> throw $ ParseError 0 "Debuging Required"
    Fail left ps p ->
        let totalLineno = length $ B.lines contents
            unparsed = B.lines left
            lineAfterError = "... " `B.append` head unparsed
            unparsedLineno = totalLineno - length unparsed + 1
        in throw $ ParseError unparsedLineno lineAfterError
    where parseDone p c = feed (parse p c) B.empty


-- Object body never be empty
object :: Parser Object
object = do
    name  <- "H" *> P.take 6
    start <- word
    len   <- word <* endOfLine
    body  <- textrc `sepBy1` endOfLine
    endOfLine
    boot  <- "E" *> word
    option () endOfLine *> endOfInput
    return $ Object name start boot len body
  <?> "newline mismatch"

-- TextRecord contents never be empty
textrc :: Parser TextRecord
textrc = do
   char 'T'
   start <- word -- Address
   len <- byte -- Int
   objs <- count len byte <?> show len ++ " bytes in text"-- Word8
           -- ensures size of textrecord
   return $ TextRecord start objs

word :: (Integral a, Bits a) => Parser a
word = hexadecimal' 6

byte :: (Integral a, Bits a) => Parser a
byte = hexadecimal' 2

-- modification of hexadecimal in Data.Attoparsec.ByteString.Char8
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
