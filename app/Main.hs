module Main where

import Data.BitVector.Sized
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Map (empty)
import Data.Maybe (fromMaybe)
import Data.Parameterized (sndPair, viewSome)
import Data.Text (unpack)
import Elf.Elf
import Elf.Types (EiData (..), Elf (..))
import Emulator (Memory, PC, Registers, run, setMem)
import Instructions (Size, size)

bsChunks :: Int64 -> ByteString -> [ByteString]
bsChunks _ "" = []
bsChunks n bs = BS.take n bs : bsChunks n (BS.drop n bs)

code :: Elf -> [BV Size]
code e@(Elf _ d _) = map (viewSome (\(BV n) -> mkBV (knownNat @Size) n) . sndPair . construct) $ bsChunks (fromIntegral size `div` 8) $ elfTextSection e
  where
    construct = case d of
      ElfData2Lsb -> bytestringLE . toStrict
      ElfData2Msb -> bytestringBE . toStrict

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
    Right b -> do
      let c = code b
      print c
      let w = concatMap (fromMaybe undefined . asBytesLE (knownNat @Size)) c
      let mem = setMem initMem (zero (knownNat @Size)) w
      print $ run (mem, initRegs, initPC)
