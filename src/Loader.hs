
module Loader
    ( loader
    , loadObj
    , loadTextrc
    , checkTextOverlap
    ) where

import System.IO
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.List (sort)
import Control.Exception (throw)

import BasicTypes
import Cpu.Types (Memory, setBytes)
import Loader.Types
import Loader.Parser


-- | Load SIC object to given memory.
loader :: ByteString        -- ^ SIC object file content to load on memry.
       -> Memory            -- ^ Memory to load object file on.
       -> (Memory, Address) -- ^ Resulting memry and the booting address.
loader contents mem = flip loadObj mem $ parseHandler object contents

-- | Load each text record in object to given memory.
-- If two of the text records overap, it rasises 'TextRecordOverlap'.
loadObj :: Object -> Memory -> (Memory, Address)
loadObj obj@(Object _ _ boot _ texts) mem
    | not $ checkTextOverlap obj = throw TextRecordOverlap
    | otherwise = (foldl (flip loadTextrc) mem texts, boot)

-- | Loads byte codes in text record to given memory.
loadTextrc :: TextRecord -> Memory -> Memory
loadTextrc (TextRecord start bytes) mem = setBytes start bytes mem

-- | @checkTextOverlap obj@ returns whether 'TextRecord's in @obj@
-- overlapps each other.
checkTextOverlap :: Object -> Bool
checkTextOverlap (Object _ _ _ _ ts) =
    let segs = (0, 0) : (sort . map refine) ts
    in and $ zipWith (\(s1,l1) (s2,_) -> s1+l1 <= s2) segs (tail segs)
    -- segs will never be empty -> tail always succeed
    where refine (TextRecord s ls) = (fromIntegral s, length ls)
