module Elf.Utils where

import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Word (Word16, Word32)
import Elf.Types
import Types

eiClass :: ByteString -> Architecture
eiClass e = case BS.index e 4 of
  1 -> A32
  2 -> A64
  _ -> undefined

eiData :: ByteString -> Endianness
eiData e = case BS.index e 5 of
  1 -> LE
  2 -> BE
  _ -> undefined

getUint16From :: Endianness -> Int64 -> ByteString -> Word16
getUint16From d i e = case d of
  LE -> runGet getWord16le w
  BE -> runGet getWord16be w
  where
    w = BS.take (16 `div` 8) $ BS.drop i e

getUint32From :: Endianness -> Int64 -> ByteString -> Word32
getUint32From d i e = case d of
  LE -> runGet getWord32le w
  BE -> runGet getWord32be w
  where
    w = BS.take (32 `div` 8) $ BS.drop i e

getUintClassFrom :: Architecture -> Endianness -> Int64 -> ByteString -> Either3264
getUintClassFrom c d i e = case c of
  A32 -> case d of
    LE -> E32 $ runGet getWord32le w
    BE -> E32 $ runGet getWord32be w
  A64 -> case d of
    LE -> E64 $ runGet getWord64le w
    BE -> E64 $ runGet getWord64be w
  where
    w = BS.take (architectureToInt c `div` 8) $ BS.drop i e
