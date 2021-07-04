
module Main where

import System.IO
import qualified Data.ByteString as B
import Data.List (dropWhileEnd)

import Assembler (assembler)
import Loader

import Cpu
import Cpu.Types
import Cpu.Debugger

runAssembler :: FilePath -> IO ()
runAssembler filepath = do
    contents <- B.readFile filepath
    let outfile = dropWhileEnd (/='.') filepath ++ "obj"
    let objxt = assembler contents
    B.writeFile outfile objxt

runLoader :: FilePath -> Memory -> IO (Memory, Address)
runLoader filepath mem = flip loader mem <$> B.readFile filepath

main :: IO ()
main = do
    runAssembler "sample4.asm"
    let clean_mem = fixedMemory 2000
    (mem, pcv) <- runLoader "sample4.obj" clean_mem
    let ds = [(6, Device NullIn (Dump "dev6"))]
    let st = setR pc pcv $ cpuState mem emptyRegister ds
    (ST mem reg _) <- debugCpu st
    print mem
    print reg
    return ()
