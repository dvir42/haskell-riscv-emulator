module Elf.Utils where

import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Word (Word16, Word32)
import Elf.Types

eiClass :: ByteString -> EiClass
eiClass e = case BS.index e 4 of
  1 -> ElfClass32
  2 -> ElfClass64
  _ -> undefined

eiData :: ByteString -> EiData
eiData e = case BS.index e 5 of
  1 -> ElfData2Lsb
  2 -> ElfData2Msb
  _ -> undefined

eiClassToInt :: EiClass -> Int64
eiClassToInt ElfClass32 = 32
eiClassToInt ElfClass64 = 64

getUint16From :: EiData -> Int64 -> ByteString -> Word16
getUint16From d i e = case d of
  ElfData2Lsb -> runGet getWord16le w
  ElfData2Msb -> runGet getWord16be w
  where
    w = BS.take (16 `div` 8) $ BS.drop i e

getUint32From :: EiData -> Int64 -> ByteString -> Word32
getUint32From d i e = case d of
  ElfData2Lsb -> runGet getWord32le w
  ElfData2Msb -> runGet getWord32be w
  where
    w = BS.take (32 `div` 8) $ BS.drop i e

getUintClassFrom :: EiClass -> EiData -> Int64 -> ByteString -> Either3264
getUintClassFrom c d i e = case c of
  ElfClass32 -> case d of
    ElfData2Lsb -> E32 $ runGet getWord32le w
    ElfData2Msb -> E32 $ runGet getWord32be w
  ElfClass64 -> case d of
    ElfData2Lsb -> E64 $ runGet getWord64le w
    ElfData2Msb -> E64 $ runGet getWord64be w
  where
    w = BS.take (eiClassToInt c `div` 8) $ BS.drop i e
