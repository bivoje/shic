{-# LANGUAGE FlexibleContexts #-}

-- TODO

-- |
--
-- RISC CPU usually have pipelining.
--
-- IF ID EX MEM WB
--
-- in this SIC implementation we use
--
-- IF ID and EX (includes MEM and WB)
--

module Cpu
    ( runCpu
    , run
    , execute
    , decode
    , fetch
    ) where


import Prelude hiding (Word)

import Data.Bits
import Data.HashMap.Strict as H
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Control.Lens

import Control.Monad (when)

import Cpu.Types


-- | Fetch an instruction bytecode from memory.
fetch :: (MonadState ST (t IO), MonadTrans t) => t IO Word
fetch = do
    pcv <- gets $ viewR pc
    gets $ getWord pcv . view memory

-- | Decode given instruction bytecode.
decode :: Word -> (Opcode, Bool, Address)
decode word =
    let x = (1 ==) $ (word `shiftL` 15) .&. 0x1
        ta = fromIntegral $ word .&. 0x7FFF
    in case fromOp $ lowBits 8 (word `shiftR` 16) of
        Just opcode -> (opcode, x, ta)
        Nothing -> throw $ InvalidOpcode word

-- | Execute given instruction.
execute :: (MonadState ST (t IO), MonadTrans t)
        => Opcode -> Address -> t IO ()
execute opcode addr = case opcode of
    LDA  -> loadR a
    LDX  -> loadR x
    LDL  -> loadR l
    STA  -> storeR a
    STX  -> storeR x
    STL  -> storeR l
    ADD  -> modifyA (+)
    SUB  -> modifyA $ flip (-)
    MUL  -> modifyA (*)
    DIV  -> modifyA $ flip div
    COMP -> getR a `comp` getM
    TIX  -> do
        modify $ overR x (+1)
        getR x `comp` getM
    JEQ  -> jumpIf EQ
    JGT  -> jumpIf GT
    JLT  -> jumpIf LT
    J    -> putR pc addr
    AND  -> modifyA (.&.)
    OR   -> modifyA (.|.)
    JSUB -> moveR pc l >> putR pc addr
    RSUB -> moveR l pc
    LDCH -> do
        ch <- gets (getByte addr . view memory)
        let wch = lowBits 8 ch
        putR a wch
    STCH -> do
        wch <- getR a
        let ch = lowBits 8 wch
        modify $ over memory (setByte addr ch)
    STSW -> storeR sw
    RD -> do
        d <- gets $ getByte addr . view memory
        dev <- getD d
        (ch, ndev) <- lift $ readDev dev
        let wch = lowBits 8 ch
        modify $ setR a wch
        setD d ndev
    WD -> do
        d <- gets $ getByte addr . view memory
        dev <- getD d
        wch <- gets $ viewR a
        let ch = lowBits 8 wch
        ndev <- lift $ writeDev ch dev
        setD d ndev
    TD -> do
        d <- gets $ getByte addr . view memory
        dev <- getD d
        succ <- lift $ testDev dev
        let ret = if succ then 0x80 else 0x00
        modify $ setR sw ret
  where
    faild = Device FailIn FailOut
    getD d = gets $ (H.lookupDefault faild d) . view device
    setD d dev = modify $ set (device.at d) (Just dev)
    getM = gets $ getWord addr . view memory
    storeM word = modify $ over memory (setWord addr word)
    getR reg = gets $ viewR reg
    putR reg word = modify $ setR reg word
    loadR reg = getM >>= putR reg
    storeR reg = getR reg >>= storeM
    moveR r1 r2 = getR r1 >>= putR r2
    modifyA f = getM >>= \w -> modify $ overR a (f w)
    conditionCode stat = case stat of
        LT -> 0x40
        EQ -> 0x00
        GT -> 0x80
    ordering code = case code of
        0x40 -> LT
        0x00 -> EQ
        0x80 -> GT
    comp x y = do
        xVal <- x
        yVal <- y
        putR sw . conditionCode $ xVal `compare` yVal
    jumpIf ord = do
        stat <- ordering <$> getR sw
        when (stat == ord) $ modify (setR pc addr)

-- | Perform a instruction pipeline (1 cycle).
-- Return @False@ if halt.
run :: (MonadState ST (t IO), MonadTrans t)
    => t IO Bool
run = do
    word <- fetch
    let (opcode, x', ta') = decode word
    xVal <- gets $ viewR x
    let ta = ta' + (if x' then xVal else 0)
    modify $ overR pc (+3)
    if word == haltcode
     then return False
     else execute opcode ta >> return True
    where haltcode = 0x3C0000 --0x3F2FFD

-- | Run Cpu with given state until it halts.
runCpu :: ST -> IO ST
runCpu st = execStateT (whileM_ run) st

-- Reimplementation of whileM of Control.Monad.Extra in extra package.
whileM_ :: Monad m => m Bool -> m ()
whileM_ act = act >>= \b -> when b $ whileM_ act
