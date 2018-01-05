{-# LANGUAGE FlexibleContexts #-}

module Cpu where


import Prelude hiding (Word)

import Data.Bits
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as H
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Control.Lens

import Control.Monad (when)

import Cpu.Types


optbl :: HashMap Byte Opcode
optbl = H.fromList
    [ (0x00, LDA  )
    , (0x04, LDX  )
    , (0x08, LDL  )
    , (0x0C, STA  )
    , (0x10, STX  )
    , (0x14, STL  )
    , (0x18, ADD  )
    , (0x1C, SUB  )
    , (0x20, MUL  )
    , (0x24, DIV  )
    , (0x28, COMP )
    , (0x2C, TIX  )
    , (0x30, JEQ  )
    , (0x34, JGT  )
    , (0x38, JLT  )
    , (0x3C, J    )
    , (0x40, AND  )
    , (0x44, OR   )
    , (0x48, JSUB )
    , (0x4C, RSUB )
    , (0x50, LDCH )
    , (0x54, STCH )
    , (0xD8, RD   )
    , (0xDC, WD   )
    , (0xE0, TD   )
    , (0xE8, STSW )
    ]

fetch :: (MonadState ST (t IO), MonadTrans t) => t IO Word
fetch = do
    pcv <- gets $ viewR pc
    gets $ getWord pcv . view memory

decode :: Word -> (Opcode, Bool, Address)
decode word =
    let x = (1 ==) $ (word `shiftL` 15) .&. 0x1
        ta = fromIntegral $ word .&. 0x7FFF
    in case flip H.lookup optbl $ lowBits 8 (word `shiftR` 16) of
        Just opcode -> (opcode, x, ta)
        Nothing -> throw $ InvalidOpcode word

execute :: (MonadState ST (t IO), MonadTrans t)
        => Opcode -> Address -> t IO ()
execute opcode addr = case opcode of
    RD -> do
        d   <- gets $ getByte addr . view memory
        dev <- gets $ ($ d) . view device
        ch  <- lift $ readByte dev
        let wch = lowBits 8 ch
        modify $ setR a wch
    WD -> do
        d   <- gets $ getByte addr . view memory
        dev <- gets $ ($ d) . view device
        wch <- gets $ viewR a
        let ch = lowBits 8 wch
        lift $ writeByte dev ch
    TD -> modify $ setR sw 0x80 -- always good to go
    _  -> execute' opcode addr

execute' :: (MonadState ST (t IO), MonadTrans t)
         => Opcode -> Address -> t IO ()
execute' opcode addr = case opcode of
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
  where
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

-- returns False if halt
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

runCpu :: ST -> IO ST
runCpu st = execStateT (whileM_ run) st

whileM_ :: Monad m => m Bool -> m ()
whileM_ act = act >>= \b -> when b $ whileM act
