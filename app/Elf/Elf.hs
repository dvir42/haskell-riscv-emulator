module Elf.Elf where

import Data.ByteString.Lazy (ByteString, hGetContents)
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Elf.Header
import Elf.Sections (sections, shOffset, shSize)
import Elf.Types
import Elf.Utils
import System.IO (IOMode (ReadMode), openFile)

elfContents :: FilePath -> IO (Either Text Elf)
elfContents path = do
  h <- openFile path ReadMode
  contents <- hGetContents h
  if isElf contents
    then return $ Right $ Elf (eiClass contents) (eiData contents) contents
    else return $ Left $ pack "Invalid ELF file"

elfTextSection :: Elf -> ByteString
elfTextSection e =
  BS.take (fromIntegral $ shSize e off) $
    BS.drop (fromIntegral $ shOffset e off) $
      content e
  where
    off = fromIntegral $ fromMaybe (error "No .text section") $ elemIndex ".text" $ sections e
