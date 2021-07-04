{-# LANGUAGE TemplateHaskell #-}

module Cpu.Debugger.Types where


import Prelude hiding (Word)

import Control.Lens
import Data.Bits
import Data.Set (Set)
import qualified Data.Set as S

import BasicTypes

-- FIXME separate breakpoint info
-- | Indicate whether to run cpu cycles.
data GoSign
    = Quit -- ^ quit debugger, interactive-only sign
    | Term -- ^ program execution terminated, run-only sign
    | Stop -- ^ stop and show interactive session
    | CountDown Int -- ^ run @n@ steps and stop.
    | EqWReg Bool Int Word  -- ^ stop when value of @i@th register
                            -- ^ is equal (or not depending on boolean)
                            -- ^ to @word@.
    deriving (Show, Eq)

-- | Debugger runtime state.
data Info = Info
    { _memPicks :: Set Address
    } deriving Show -- FIXME enable cycle count

makeLenses ''Info

refineAddr :: Address -> Address
refineAddr = flip shiftL 4 . flip shiftR 4

insertMP :: Address -> Set Address -> Set Address
insertMP addr = S.insert (refineAddr addr)

deleteMP :: Address -> Set Address -> Set Address
deleteMP addr = S.delete (refineAddr addr)


data Command
    = Empty
    | Exit
    | Step Int
    | BreakAt (Maybe Address)
    | WatchWReg Bool Int Word
    | Mempick Bool Address
    deriving Show
