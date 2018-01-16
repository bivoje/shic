
module Main where


import Assembler
import Loader
import Cpu
import Cpu.Types
import Cpu.Debugger


main :: IO ()
main = do
    runAssembler "sample4.asm"
    let clean_mem = fixedMemory 350
    (mem, pcv) <- loadFile "sample4.obj" clean_mem
    let st = setR pc pcv $ cpuState mem []
    (ST mem reg _) <- runCpu st
    --(ST mem reg _) <- debugCpu st
    print mem
    print reg
    return ()
