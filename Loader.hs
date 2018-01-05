
module Loader (loadFile, parseHandler) where


import System.IO
import qualified Data.ByteString as B

import Data.List (sort)
import Control.Exception (throw)

import BasicTypes
import Cpu.Types (Memory, setBytes)
import Loader.Types
import Loader.Parser


loadFile :: FilePath -> Memory -> IO (Memory, Address)
loadFile filepath mem = withFile filepath ReadMode (\h -> do
    contents <- B.hGetContents h
    return . flip loadObj mem . parseHandler object $ contents
  )

loadObj :: Object -> Memory -> (Memory, Address)
loadObj obj@(Object _ _ boot _ texts) mem
    | not $ checkTextOverlap obj = throw TextRecordOverlap
    | otherwise = (foldl (flip loadTextrc) mem texts, boot)

loadTextrc :: TextRecord -> Memory -> Memory
loadTextrc (TextRecord start bytes) mem = setBytes start bytes mem

checkTextOverlap :: Object -> Bool
checkTextOverlap (Object _ _ _ _ ts) =
    let segs = sort $ map (\(TextRecord s ls) ->
                                (fromIntegral s, length ls)) ts
    in and $ zipWith (\(s1,l1) (s2,_) -> s1+l1 <= s2) segs (tail segs)
    -- ts will never be empty -> tail always succeed
