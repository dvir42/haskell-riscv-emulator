module Emulator.State where

import Control.Lens
import Data.BitVector.Sized (BV, NatRepr, asUnsigned, knownNat, mkBV)
import qualified Data.BitVector.Sized as BV
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Parameterized (Pair, Some, sndPair, viewSome)
import Data.Word (Word8)
import Instructions
import Registers
import Types

type Address = BV Size

type Memory = ByteString

type Registers = Map Register (BV Size)

type PC = BV Size

newtype Params = Params {endianness :: Endianness}

type State = (Memory, Registers, PC, Params)

zero :: BV Size
zero = BV.zero $ knownNat @Size

one :: BV Size
one = BV.one $ knownNat @Size

viewSomeBV :: NatRepr w -> Some BV -> BV w
viewSomeBV w = viewSome (\(BV.BV n) -> mkBV w n)

getMem :: (ByteString -> Pair NatRepr BV) -> Memory -> Int64 -> Address -> Some BV
getMem f m n a = sndPair $ f $ BS.take n $ BS.drop (fromIntegral $ asUnsigned a) m

setMem :: Memory -> Address -> [Word8] -> Memory
setMem m _ [] = m
setMem m a (w : ws) =
  setMem
    (m & ix (fromIntegral $ asUnsigned a) .~ w)
    (BV.add (knownNat @Size) a one)
    ws

getReg :: Registers -> Register -> BV Size
getReg rs r = fromMaybe zero $ M.lookup r rs

setReg :: Registers -> Register -> BV Size -> Registers
setReg rs r v = M.insert r v rs

incPC :: PC -> PC
incPC pc = BV.add (knownNat @Size) pc $ mkBV (knownNat @Size) $ size `div` 8
