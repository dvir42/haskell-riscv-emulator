module Main where

import Control.Monad (void)
import Data.BitVector.Sized
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Map (empty)
import Data.Maybe (fromMaybe)
import Data.Parameterized (sndPair, viewSome)
import Data.Text (unpack)
import Elf.Elf
import Elf.Types (Elf (..))
import Emulator.Emulator (run)
import Emulator.State (Memory, PC, Params (Params, endianness), Registers, setMem)
import Instructions (Size, size)
import Types

bsChunks :: Int64 -> ByteString -> [ByteString]
bsChunks _ "" = []
bsChunks n bs = BS.take n bs : bsChunks n (BS.drop n bs)

code :: Elf -> [BV Size]
code e@(Elf _ d _) =
  map
    (viewSome (\(BV n) -> mkBV (knownNat @Size) n) . sndPair . construct)
    $ bsChunks (fromIntegral size `div` 8)
    $ elfTextSection e
  where
    construct = case d of
      LE -> bytestringLE . toStrict
      BE -> bytestringBE . toStrict

initMem :: Memory
initMem = BS.replicate 1024 0

initRegs :: Registers
initRegs = empty

initPC :: PC
initPC = zero $ knownNat @Size

main :: IO ()
main = do
  e <- elfContents "/home/dvir/code/riscv/hello"
  case e of
    Left err -> error $ "error getting elf contents: " <> unpack err
    Right elf@(Elf _ d _) -> do
      let bytes =
            concatMap
              (fromMaybe undefined . asBytesLE (knownNat @Size))
              $ code elf
      let mem = setMem initMem (zero (knownNat @Size)) bytes
      let params = Params {endianness = d}
      void $ run (mem, initRegs, initPC, params)
