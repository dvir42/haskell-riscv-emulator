module Elf.Types where

import Control.Lens
import Data.ByteString.Builder (byteStringHex)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

data EiClass = ElfClass32 | ElfClass64 deriving (Show)

data EiData = ElfData2Lsb | ElfData2Msb deriving (Show)

data Elf = Elf EiClass EiData ByteString

instance Show Elf where
  show (Elf c d e) =
    "ELF: Class "
      <> show c
      <> ", Data "
      <> show d
      <> "\n"
      <> show (byteStringHex $ toStrict e)

content :: Elf -> ByteString
content (Elf _ _ e) = e

class (Integral n, Show n) => Elf32 n

class (Integral n, Show n) => Elf64 n

instance Elf32 Word32

instance Elf32 Int32

instance Elf64 Word64

instance Elf64 Int64

data Either3264 where
  E32 :: Elf32 n => n -> Either3264
  E64 :: Elf64 n => n -> Either3264

instance Show Either3264 where
  show (E32 n) = "E32 " <> show n
  show (E64 n) = "E64 " <> show n

instance Enum Either3264 where
  toEnum = toEnum

  fromEnum (E32 n) = fromEnum n
  fromEnum (E64 n) = fromEnum n

instance Num Either3264 where
  abs (E32 n) = E32 $ abs n
  abs (E64 n) = E64 $ abs n

  signum (E32 n) = E32 $ signum n
  signum (E64 n) = E64 $ signum n

  fromInteger n = E64 (fromInteger n :: Int64)

  negate (E32 n) = E32 $ negate n
  negate (E64 n) = E64 $ negate n

  E32 x + E32 y = E32 (fromInteger $ toInteger x + toInteger y :: Int32)
  E64 x + E64 y = E64 (fromInteger $ toInteger x + toInteger y :: Int64)
  E32 x + E64 y = E64 (fromInteger $ toInteger x + toInteger y :: Int64)
  E64 x + E32 y = E64 (fromInteger $ toInteger x + toInteger y :: Int64)

  E32 x * E32 y = E32 (fromInteger $ toInteger x * toInteger y :: Int32)
  E64 x * E64 y = E64 (fromInteger $ toInteger x * toInteger y :: Int64)
  E32 x * E64 y = E64 (fromInteger $ toInteger x * toInteger y :: Int64)
  E64 x * E32 y = E64 (fromInteger $ toInteger x * toInteger y :: Int64)

instance Eq Either3264 where
  E32 x == E32 y = toInteger x == toInteger y
  E64 x == E64 y = toInteger x == toInteger y
  E32 x == E64 y = toInteger x == toInteger y
  E64 x == E32 y = toInteger x == toInteger y

instance Ord Either3264 where
  E32 x <= E32 y = toInteger x <= toInteger y
  E64 x <= E64 y = toInteger x <= toInteger y
  E32 x <= E64 y = toInteger x <= toInteger y
  E64 x <= E32 y = toInteger x <= toInteger y

instance Real Either3264 where
  toRational (E32 n) = toRational n
  toRational (E64 n) = toRational n

instance Integral Either3264 where
  toInteger (E32 n) = toInteger n
  toInteger (E64 n) = toInteger n

  quotRem (E32 x) (E32 y) = over both E32 (over both fromInteger (quotRem (toInteger x) (toInteger y)) :: (Int32, Int32))
  quotRem (E64 x) (E64 y) = over both E64 (over both fromInteger (quotRem (toInteger x) (toInteger y)) :: (Int64, Int64))
  quotRem (E32 x) (E64 y) = over both E64 (over both fromInteger (quotRem (toInteger x) (toInteger y)) :: (Int64, Int64))
  quotRem (E64 x) (E32 y) = over both E64 (over both fromInteger (quotRem (toInteger x) (toInteger y)) :: (Int64, Int64))
