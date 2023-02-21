
module Main where

import Text.Read (readMaybe)
import System.IO
import System.Environment
import qualified Data.ByteString as B
import Data.List (dropWhileEnd)
import Control.Monad (void)
import Control.Lens ((<&>))

import Loader
import Cpu
import Cpu.Types
import Cpu.Debugger
import Assembler (assembler)

runLoader :: FilePath -> Memory -> IO (Memory, Address)
runLoader filepath mem = flip loader mem <$> B.readFile filepath

loadDevs :: Maybe FilePath -> IO [(Byte, Device)]
loadDevs Nothing = return $ map (\i -> (i, Device ForwardIn (Dump $ "dev" ++ show i))) [0..10]
loadDevs (Just filepath) = parsed >>= maybe (fail "incompatible devs file") return
    where parsed :: IO (Maybe [(Byte, Device)])
          parsed = readFile filepath <&> traverse readMaybe . lines

main :: IO ()
main = do
    args <- getArgs
    (fpsrc, smems, mfpdev) <- case args of
        [a,b] -> putStrLn "using default devices..." >> return (a, b, Nothing)
        [a,b,c] -> return (a, b, Just c)
        _ -> fail "Usage: shic-dgb <src.obj> <memsize in bytes> [<device spec file>]"
    mems <- case reads smems of
        [(i,"")] -> return i
        _ -> fail "memsize is size of memory in bytes, must be integer"
    ds <- loadDevs mfpdev

    let clean_mem = fixedMemory mems
    (mem, pcv) <- runLoader fpsrc clean_mem
    let st = setR pc pcv $ cpuState mem emptyRegister ds
    (ST mem reg _) <- debugCpu st 
    print mem
    print reg
