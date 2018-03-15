
module Loader.Types where


import Control.Exception
import Data.ByteString (ByteString)

import BasicTypes


data InvalidObject
    = ParseError Int ByteString
    | TextRecordOverlap
    deriving Show

instance Exception InvalidObject

data TextRecord = TextRecord Address [Byte] deriving (Show, Eq)

data Object = Object ByteString Address Address Length [TextRecord]
--                   name       start   boot    length body
     deriving (Show, Eq)
