module Emulator.Syscalls where

import Data.BitVector.Sized (asSigned, asUnsigned, knownNat)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BS
import Emulator.State (State, getReg, incPC)
import Instructions (Size)
import Registers
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.Posix.ByteString (Fd (Fd), fdWrite)

write :: State -> IO ()
write (m, rs, _) = do
  let fd = fromIntegral $ asUnsigned $ getReg rs A0
  let buf = fromIntegral $ asUnsigned $ getReg rs A1
  let count = fromIntegral $ asUnsigned $ getReg rs A2
  let string = toStrict $ BS.take count $ BS.drop buf m
  _ <- fdWrite (Fd fd) string
  return ()

exit :: State -> IO ()
exit (_, rs, _) = do
  let error_code = fromIntegral $ asSigned (knownNat @Size) $ getReg rs A0
  exitWith $ if error_code == 0 then ExitSuccess else ExitFailure error_code

runCall :: State -> IO State
runCall s@(m, rs, pc) = case asUnsigned $ getReg rs A7 of
  64 -> do write s; return (m, rs, incPC pc)
  93 -> do exit s; return (m, rs, incPC pc)
  i -> error $ "undefined syscall: " <> show i
