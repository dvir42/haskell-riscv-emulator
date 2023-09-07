module Emulator.Syscalls where

import Data.BitVector.Sized (asSigned, asUnsigned, knownNat)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL
import Emulator.State (Memory, State, getReg, incPC, setMem)
import Instructions (Size)
import Registers
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.Posix.ByteString (Fd (Fd), fdRead, fdWrite)

sread :: State -> IO Memory
sread (m, rs, _) = do
  let fd = fromIntegral $ asUnsigned $ getReg rs A0
  let buf = getReg rs A1
  let count = fromIntegral $ asUnsigned $ getReg rs A2
  string <- fdRead (Fd fd) count
  return $ setMem m buf $ BS.unpack string

swrite :: State -> IO ()
swrite (m, rs, _) = do
  let fd = fromIntegral $ asUnsigned $ getReg rs A0
  let buf = fromIntegral $ asUnsigned $ getReg rs A1
  let count = fromIntegral $ asUnsigned $ getReg rs A2
  let string = toStrict $ BL.take count $ BL.drop buf m
  _ <- fdWrite (Fd fd) string
  return ()

sexit :: State -> IO ()
sexit (_, rs, _) = do
  let error_code = fromIntegral $ asSigned (knownNat @Size) $ getReg rs A0
  exitWith $ if error_code == 0 then ExitSuccess else ExitFailure error_code

runCall :: State -> IO State
runCall s@(m, rs, pc) = case asUnsigned $ getReg rs A7 of
  63 -> do
    nm <- sread s
    return (nm, rs, incPC pc)
  64 -> do swrite s; return (m, rs, incPC pc)
  93 -> do sexit s; return (m, rs, incPC pc)
  i -> error $ "undefined syscall: " <> show i
