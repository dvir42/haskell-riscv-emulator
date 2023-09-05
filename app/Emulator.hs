module Emulator where

import Control.Lens
import Data.BitVector.Sized (BV, NatRepr, asBytesLE, asUnsigned, bytestringLE, knownNat, mkBV, truncBits)
import qualified Data.BitVector.Sized as BV
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Parameterized (Pair, Some, sndPair, viewSome)
import Data.Word (Word8)
import Instructions (Immediate, Instruction (..), Size, size)
import Registers (Register)

type Address = BV Size

type Memory = ByteString

type Registers = Map Register (BV Size)

type PC = BV Size

type State = (Memory, Registers, PC)

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

run :: Instruction -> [Register] -> [Immediate] -> State -> State
run LUI [rd] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd imm,
    incPC pc
  )
run AUIPC [rd] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.add (knownNat @Size) pc imm,
    incPC pc
  )
run JAL [rd] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ incPC pc,
    BV.add (knownNat @32) pc imm
  )
run JALR [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ incPC pc,
    BV.add
      (knownNat @Size)
      pc
      $ BV.clearBit (knownNat @Size) (knownNat @0)
      $ BV.add (knownNat @Size) imm
      $ getReg rs rs1
  )
run BEQ [rs1, rs2] [imm] (m, rs, pc) =
  ( m,
    rs,
    if getReg rs rs1 == getReg rs rs2
      then BV.add (knownNat @Size) imm pc
      else incPC pc
  )
run BNE [rs1, rs2] [imm] (m, rs, pc) =
  ( m,
    rs,
    if getReg rs rs1 /= getReg rs rs2
      then BV.add (knownNat @Size) imm pc
      else incPC pc
  )
run BLT [rs1, rs2] [imm] (m, rs, pc) =
  ( m,
    rs,
    if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
      then BV.add (knownNat @Size) imm pc
      else incPC pc
  )
run BLT [rs1, rs2] [imm] (m, rs, pc) =
  ( m,
    rs,
    if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
      then BV.add (knownNat @Size) imm pc
      else incPC pc
  )
run BGE [rs1, rs2] [imm] (m, rs, pc) =
  ( m,
    rs,
    if not $ BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
      then BV.add (knownNat @Size) imm pc
      else incPC pc
  )
run BLTU [rs1, rs2] [imm] (m, rs, pc) =
  ( m,
    rs,
    if BV.ult (getReg rs rs1) (getReg rs rs2)
      then BV.add (knownNat @Size) imm pc
      else incPC pc
  )
run BGEU [rs1, rs2] [imm] (m, rs, pc) =
  ( m,
    rs,
    if not $ BV.ult (getReg rs rs1) (getReg rs rs2)
      then BV.add (knownNat @Size) imm pc
      else incPC pc
  )
run LB [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.sext (knownNat @8) (knownNat @Size) $
        viewSomeBV (knownNat @8) $
          getMem (bytestringLE . toStrict) m 1 $
            BV.add (knownNat @Size) imm $
              getReg rs rs1,
    incPC pc
  )
run LH [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.sext (knownNat @16) (knownNat @Size) $
        viewSomeBV (knownNat @16) $
          getMem (bytestringLE . toStrict) m 2 $
            BV.add (knownNat @Size) imm $
              getReg rs rs1,
    incPC pc
  )
run LW [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      viewSomeBV (knownNat @Size) $
        getMem (bytestringLE . toStrict) m 4 $
          BV.add (knownNat @Size) imm $
            getReg rs rs1,
    incPC pc
  )
run LBU [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.zext (knownNat @Size) $
        viewSomeBV (knownNat @8) $
          getMem (bytestringLE . toStrict) m 1 $
            BV.add (knownNat @Size) imm $
              getReg rs rs1,
    incPC pc
  )
run LHU [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.zext (knownNat @Size) $
        viewSomeBV (knownNat @16) $
          getMem (bytestringLE . toStrict) m 2 $
            BV.add (knownNat @Size) imm $
              getReg rs rs1,
    incPC pc
  )
run SB [rs1, rs2] [imm] (m, rs, pc) =
  ( setMem
      m
      (BV.add (knownNat @Size) imm $ getReg rs rs1)
      ( fromMaybe undefined $
          asBytesLE (knownNat @8) $
            BV.trunc (knownNat @8) $
              getReg rs rs2
      ),
    rs,
    incPC pc
  )
run SH [rs1, rs2] [imm] (m, rs, pc) =
  ( setMem
      m
      (BV.add (knownNat @Size) imm $ getReg rs rs1)
      ( fromMaybe undefined $
          asBytesLE (knownNat @16) $
            BV.trunc (knownNat @16) $
              getReg rs rs2
      ),
    rs,
    incPC pc
  )
run SW [rs1, rs2] [imm] (m, rs, pc) =
  ( setMem
      m
      (BV.add (knownNat @Size) imm $ getReg rs rs1)
      ( fromMaybe undefined $
          asBytesLE (knownNat @Size) $
            getReg rs rs2
      ),
    rs,
    incPC pc
  )
run ADDI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.add (knownNat @Size) (getReg rs rs1) imm,
    incPC pc
  )
run SLTI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ if BV.slt (knownNat @Size) (getReg rs rs1) imm then one else zero,
    incPC pc
  )
run SLTIU [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ if BV.ult (getReg rs rs1) imm then one else zero,
    incPC pc
  )
run XORI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.xor (getReg rs rs1) imm,
    incPC pc
  )
run ANDI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.and (getReg rs rs1) imm,
    incPC pc
  )
run ORI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.or (getReg rs rs1) imm,
    incPC pc
  )
run SLLI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.shl (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
    incPC pc
  )
run SRLI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.lshr (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
    incPC pc
  )
run SRAI [rd, rs1] [imm] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.ashr (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
    incPC pc
  )
run ADD [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.add (knownNat @Size) (getReg rs rs1) (getReg rs rs2),
    incPC pc
  )
run SUB [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.sub (knownNat @Size) (getReg rs rs1) (getReg rs rs2),
    incPC pc
  )
run SLL [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.shl
        (knownNat @Size)
        (getReg rs rs1)
        (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
    incPC pc
  )
run SLT [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $
      if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
        then one
        else zero,
    incPC pc
  )
run SLTU [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $ if BV.ult (getReg rs rs1) (getReg rs rs2) then one else zero,
    incPC pc
  )
run XOR [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.xor (getReg rs rs1) (getReg rs rs2),
    incPC pc
  )
run SRL [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.lshr
        (knownNat @Size)
        (getReg rs rs1)
        (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
    incPC pc
  )
run SRA [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $
      BV.ashr
        (knownNat @Size)
        (getReg rs rs1)
        (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
    incPC pc
  )
run AND [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.and (getReg rs rs1) (getReg rs rs2),
    incPC pc
  )
run OR [rd, rs1, rs2] [] (m, rs, pc) =
  ( m,
    setReg rs rd $ BV.or (getReg rs rs1) (getReg rs rs2),
    incPC pc
  )
run FENCE _ _ _ = undefined
run ECALL [] [] (m, rs, pc) = error $ show m <> "\n" <> show rs <> "\n" <> show pc
run EBREAK _ _ _ = undefined
run _ _ _ _ = undefined
