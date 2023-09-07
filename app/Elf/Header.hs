module Elf.Header where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Word (Word16)
import Elf.Types
import Elf.Utils
import Types

elfMagic :: ByteString
elfMagic = BS.pack [0x7f, 0x45, 0x4c, 0x46]

isElf :: ByteString -> Bool
isElf contents = BS.take 4 contents == elfMagic

eIdent :: ByteString -> ByteString
eIdent = BS.take 64

ePhoff :: Elf -> Either3264
ePhoff (Elf c d e) = getUintClassFrom c d phoffIndex e
  where
    phoffIndex = 16 + 2 + 2 + 4 + (architectureToInt c `div` 8)

eShoff :: Elf -> Either3264
eShoff (Elf c d e) = getUintClassFrom c d shoffIndex e
  where
    shoffIndex = 16 + 2 + 2 + 4 + (architectureToInt c `div` 8) * 2

ePhentSize :: Elf -> Word16
ePhentSize (Elf c d e) = getUint16From d phentIndex e
  where
    phentIndex = 16 + 2 + 2 + 4 + (architectureToInt c `div` 8) * 3 + 4 + 2

ePhnum :: Elf -> Word16
ePhnum (Elf c d e) = getUint16From d phnumIndex e
  where
    phnumIndex = 16 + 2 + 2 + 4 + (architectureToInt c `div` 8) * 3 + 4 + 2 + 2

eShentSize :: Elf -> Word16
eShentSize (Elf c d e) = getUint16From d shentIndex e
  where
    shentIndex = 16 + 2 + 2 + 4 + (architectureToInt c `div` 8) * 3 + 4 + 2 + 2 + 2

eShnum :: Elf -> Word16
eShnum (Elf c d e) = getUint16From d shnumIndex e
  where
    shnumIndex = 16 + 2 + 2 + 4 + (architectureToInt c `div` 8) * 3 + 4 + 2 + 2 + 2 + 2

eShstrndx :: Elf -> Word16
eShstrndx (Elf c d e) = getUint16From d shstrndxIndex e
  where
    shstrndxIndex = 16 + 2 + 2 + 4 + (architectureToInt c `div` 8) * 3 + 4 + 2 + 2 + 2 + 2 + 2
