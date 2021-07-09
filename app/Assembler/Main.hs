
module Main where


import System.IO
import System.Environment
import qualified Data.ByteString as B
import Data.List (dropWhileEnd)
import Control.Monad (void)
import Control.Lens ((<&>))

import Assembler (assembler)


main :: IO ()
main = do
    args <- getArgs
    (inpath, outpath) <- case args of
        [a,b] -> return (a,b)
        _ -> fail "Usage: shic-asm <src.asm> <dst.obj>"
    contents <- B.readFile inpath
    let objxt = assembler contents
    B.writeFile outpath objxt
