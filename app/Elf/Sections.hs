module Elf.Sections where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeASCII)
import Data.Word (Word16, Word32)
import Elf.Header
import Elf.Types
import Elf.Utils
import Types

shName :: Elf -> Word16 -> Text
shName elf@(Elf _ d e) n =
  toStrict $
    decodeASCII $
      runGet getLazyByteStringNul $
        BS.drop
          ( fromIntegral (shOffset elf (eShstrndx elf))
              + fromIntegral (getUint32From d (fromIntegral (offset n)) e)
          )
          e
  where
    offset i = eShoff elf + fromIntegral (i * eShentSize elf)

shType :: Elf -> Word16 -> Word32
shType elf@(Elf _ d e) n =
  getUint32From d (fromIntegral (offset n) + 4) e
  where
    offset i = eShoff elf + fromIntegral (i * eShentSize elf)

shOffset :: Elf -> Word16 -> Either3264
shOffset elf@(Elf c d e) n =
  getUintClassFrom c d (fromIntegral (offset n) + 4 + 4 + (architectureToInt c `div` 8) * 2) e
  where
    offset i = eShoff elf + fromIntegral (i * eShentSize elf)

shSize :: Elf -> Word16 -> Either3264
shSize elf@(Elf c d e) n =
  getUintClassFrom c d (fromIntegral (offset n) + 4 + 4 + (architectureToInt c `div` 8) * 3) e
  where
    offset i = eShoff elf + fromIntegral (i * eShentSize elf)

sections :: Elf -> [Text]
sections e = map (shName e) [0 .. eShnum e - 1]
