
module Cpu.Debugger where


import Prelude hiding (Word)

import Data.Bits
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Control.Lens hiding (Empty)

import Data.Functor.Identity (runIdentity)
import Text.Printf (printf)

import Cpu.Debugger.Types
import Cpu.Debugger.Parser
import Cpu.Types
import Cpu


debugCpu :: ST -> IO ST
debugCpu st =
    let info = execStateT (whileM_ $ debug >> lift' run) st
     in evalStateT info $ Info S.empty
     where lift' = mapStateT lift

debug :: StateT ST (StateT Info IO) ()
debug = get >>= lift . debug'

debug' :: ST -> StateT Info IO ()
debug' st@(ST mem reg dev) = do
    let pcv = _pc reg
    word <- lift $ evalStateT fetch st
    let (opcode, x, ta') = decode word
    let ta = ta' + (if x then 0 else 1)
    mps <- gets $ ins pcv .  ins ta . _memPicks
    lift $ do
        printf "\n"
        dumpRegister reg
        dumpMemory mps mem
        dumpInstr pcv word
        printf "cmd: "
    handleCommand
    where ins a = S.insert (refineAddr a)

handleCommand :: StateT Info IO ()
handleCommand = do
    line <- lift $ getLine
    case parse line of
        Just cmd -> execCommand cmd
        Nothing -> lift $ printf "command parsing failed\n"

execCommand :: Command -> StateT Info IO ()
execCommand Empty = return ()
execCommand (Mempick b addr) =
    let f = if b then insertMP else deleteMP
    in modify $ over memPicks (f addr)

dumpMemory :: Set Address -> Memory -> IO ()
dumpMemory addrs' mem = do
    let addrs = S.toList addrs'
    printf "Memory ==========\n"
    printf "            "
    printf "0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n"
    forM_ addrs $ printMemory mem

printMemory :: Memory -> Address -> IO ()
printMemory mem addr = do
    printf "  %06X | " addr
    let bs = getBytes addr 16 mem
    forM_ bs (printf "%02X ")
    printf "\n"

dumpRegister :: Register -> IO ()
dumpRegister (Register av xv lv pcv swv) = do
    printf "Registers =======\n"
    printf "  a:  %06X\tx:  %06X\tl: %06X\n" av xv lv
    printf "  pc: %06X\tsw: %06X (%s)\n" pcv swv (cond swv)
    where cond :: Word -> String
          cond code = case code of
            0x40 -> "LT"
            0x00 -> "EQ"
            0x80 -> "GT"
            _ -> "??"

dumpInstr :: Address -> Word -> IO ()
dumpInstr pc word =
    let (op, x, addr) = decode word
    in printf "Instruction =====\n  (%06X) %06X =  %s %c %04X\n"
              pc word (show op) (if x then 'x' else ' ') addr
