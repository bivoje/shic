
module Main where

import System.IO
import System.Environment
import qualified Data.ByteString as B
import Data.List (dropWhileEnd)
import Control.Monad (void)

import Assembler (assembler)
import Loader

import Cpu
import Cpu.Types
import Cpu.Debugger

runAssembler :: FilePath -> IO FilePath
runAssembler filepath = do
    contents <- B.readFile filepath
    let outfile = dropWhileEnd (/='.') filepath ++ "obj"
    let objxt = assembler contents
    B.writeFile outfile objxt
    return outfile

runLoader :: FilePath -> Memory -> IO (Memory, Address)
runLoader filepath mem = flip loader mem <$> B.readFile filepath

main :: IO ()
main = do
    args <- getArgs
    src <- case args of
        [src] -> return src
        _ -> fail "Specify exactly one source as an argument"
    tar <- runAssembler src
    let clean_mem = fixedMemory 2000 -- FIXME make it adjustable
    (mem, pcv) <- runLoader tar clean_mem
    let ds = map (\i -> (i, Device ForwardIn (Dump $ "dev" ++ show i))) [0..10] -- FIXME make it adjustable
    let st = setR pc pcv $ cpuState mem emptyRegister ds
    (ST mem reg _) <- debugCpu st 
    print mem
    print reg

