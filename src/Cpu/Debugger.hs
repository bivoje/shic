
module Cpu.Debugger
    ( debugCpu
    , interactive
    , interactive'
    , handleCommand
    , execCommand
    , dumpMemory
    , printMemory
    , dumpRegister
    , dumpInstr
    ) where


import Prelude hiding (Word)

import Data.Bits
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Control.Lens hiding (Empty)

import Data.Functor.Identity (runIdentity)
import Text.Printf (printf)

import System.IO (stdout, hFlush)

import Cpu.Debugger.Types
import Cpu.Debugger.Parser
import Cpu.Types
import Cpu

debugCpu :: ST -> IO ST
debugCpu st =
    let info = execStateT (whileM_ $ go Stop) st
     in evalStateT info $ Info S.empty


go :: GoSign -> StateT ST (StateT Info IO) Bool
go gosign = case gosign of
    Quit -> return False
    Term -> return False -- TODO enable re-run
    _ -> do
        gs <- go' gosign
        st <- get
        go $ adaptGoSign st gs

adaptGoSign :: ST -> GoSign -> GoSign
adaptGoSign st gs =
    case gs of
        Quit -> Quit
        Term -> Term
        Stop -> Stop
        CountDown 0 -> Stop
        CountDown n -> CountDown (n-1)
        EqWReg b rid x -> do
            let r = ridconv rid . _register $ st
            if b `xor` (r == x) then gs else Stop
    where ridconv rid = case rid of
            0 -> _a
            1 -> _x
            2 -> _l
            8 -> _pc
            9 -> _sw


go' :: GoSign -> StateT ST (StateT Info IO) GoSign
go' gosign = if gosign == Stop
         then interactive
         else cpuStep
   where cpuStep = lift' run <&> \b -> if b then gosign else Term
         lift' = mapStateT lift

interactive :: StateT ST (StateT Info IO) GoSign
interactive = get >>= lift . interactive'

interactive' :: ST -> StateT Info IO GoSign
interactive' st@(ST mem reg dev) = do
    let pcv = _pc reg
    word <- lift $ evalStateT fetch st
    let (opcode, x, ta') = decode word
    let ta = ta' + (if x then 0 else 1)
    mps <- gets $ insertMP pcv .  insertMP ta . _memPicks
    lift $ do
        printf "\n"
        dumpRegister reg
        dumpMemory mps mem
        dumpInstr pcv word
    handleCommand st

handleCommand :: ST -> StateT Info IO GoSign
handleCommand st = do
    line <- lift $ do
        printf "cmd: "
        hFlush stdout
        getLine
    case parse line of
        Just cmd -> execCommand st cmd
        Nothing -> do
            lift $ printf "command parsing failed\n"
            handleCommand st

execCommand :: ST -> Command -> StateT Info IO GoSign
execCommand st Empty = return Stop
execCommand st Exit = return Quit
execCommand st (Step n) = return $ CountDown n
execCommand st (BreakAt Nothing) =
    return . EqWReg True 8 . _pc . _register $ st
execCommand st (BreakAt (Just x)) = return $ EqWReg True 8 x
execCommand st (WatchWReg b r x) = return $ EqWReg b r x
execCommand st (Mempick b addr) =
    let f = if b then insertMP else deleteMP
    in (modify $ over memPicks (f addr)) >> return Stop

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

-- Reimplementation of whileM of Control.Monad.Extra in extra package.
whileM_ :: Monad m => m Bool -> m ()
whileM_ act = act >>= \b -> when b $ whileM_ act
