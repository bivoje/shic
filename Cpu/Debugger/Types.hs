{-# LANGUAGE TemplateHaskell #-}

module Cpu.Debugger.Types where


import Control.Lens
import Data.Bits
import Data.Set (Set)
import qualified Data.Set as S

import BasicTypes


data Info = Info
    { _memPicks :: Set Address
    } deriving Show

makeLenses ''Info

refineAddr :: Address -> Address
refineAddr = flip shiftL 4 . flip shiftR 4

insertMP :: Address -> Set Address -> Set Address
insertMP addr = S.insert (refineAddr addr)

deleteMP :: Address -> Set Address -> Set Address
deleteMP addr = S.delete (refineAddr addr)


data Command
    = Empty
    | Mempick Bool Address
    deriving Show
