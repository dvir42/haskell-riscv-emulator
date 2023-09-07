module Emulator.Emulator where

import Control.Monad.Loops (unfoldrM)
import Data.BitVector.Sized (asBytesLE, asUnsigned, bytestringLE, knownNat, truncBits)
import qualified Data.BitVector.Sized as BV
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Disassembler.Disassembler (decode)
import Emulator.State
import Emulator.Syscalls (runCall)
import Instructions (Immediate, Instruction (..), Size)
import Registers

runInstruction :: State -> Instruction -> [Register] -> [Immediate] -> IO State
runInstruction (m, rs, pc) LUI [rd] [imm] =
  return
    ( m,
      setReg rs rd imm,
      incPC pc
    )
runInstruction (m, rs, pc) AUIPC [rd] [imm] =
  return
    ( m,
      setReg rs rd $ BV.add (knownNat @Size) pc imm,
      incPC pc
    )
runInstruction (m, rs, pc) JAL [rd] [imm] =
  return
    ( m,
      setReg rs rd $ incPC pc,
      BV.add (knownNat @32) pc imm
    )
runInstruction (m, rs, pc) JALR [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ incPC pc,
      BV.add
        (knownNat @Size)
        pc
        $ BV.clearBit (knownNat @Size) (knownNat @0)
        $ BV.add (knownNat @Size) imm
        $ getReg rs rs1
    )
runInstruction (m, rs, pc) BEQ [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if getReg rs rs1 == getReg rs rs2
        then BV.add (knownNat @Size) imm pc
        else incPC pc
    )
runInstruction (m, rs, pc) BNE [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if getReg rs rs1 /= getReg rs rs2
        then BV.add (knownNat @Size) imm pc
        else incPC pc
    )
runInstruction (m, rs, pc) BLT [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc
    )
runInstruction (m, rs, pc) BLT [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc
    )
runInstruction (m, rs, pc) BGE [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if not $ BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc
    )
runInstruction (m, rs, pc) BLTU [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if BV.ult (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc
    )
runInstruction (m, rs, pc) BGEU [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if not $ BV.ult (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc
    )
runInstruction (m, rs, pc) LB [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.sext (knownNat @8) (knownNat @Size) $
          viewSomeBV (knownNat @8) $
            getMem (bytestringLE . toStrict) m 1 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc
    )
runInstruction (m, rs, pc) LH [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.sext (knownNat @16) (knownNat @Size) $
          viewSomeBV (knownNat @16) $
            getMem (bytestringLE . toStrict) m 2 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc
    )
runInstruction (m, rs, pc) LW [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        viewSomeBV (knownNat @Size) $
          getMem (bytestringLE . toStrict) m 4 $
            BV.add (knownNat @Size) imm $
              getReg rs rs1,
      incPC pc
    )
runInstruction (m, rs, pc) LBU [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.zext (knownNat @Size) $
          viewSomeBV (knownNat @8) $
            getMem (bytestringLE . toStrict) m 1 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc
    )
runInstruction (m, rs, pc) LHU [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.zext (knownNat @Size) $
          viewSomeBV (knownNat @16) $
            getMem (bytestringLE . toStrict) m 2 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc
    )
runInstruction (m, rs, pc) SB [rs1, rs2] [imm] =
  return
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
runInstruction (m, rs, pc) SH [rs1, rs2] [imm] =
  return
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
runInstruction (m, rs, pc) SW [rs1, rs2] [imm] =
  return
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
runInstruction (m, rs, pc) ADDI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.add (knownNat @Size) (getReg rs rs1) imm,
      incPC pc
    )
runInstruction (m, rs, pc) SLTI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ if BV.slt (knownNat @Size) (getReg rs rs1) imm then one else zero,
      incPC pc
    )
runInstruction (m, rs, pc) SLTIU [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ if BV.ult (getReg rs rs1) imm then one else zero,
      incPC pc
    )
runInstruction (m, rs, pc) XORI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.xor (getReg rs rs1) imm,
      incPC pc
    )
runInstruction (m, rs, pc) ANDI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.and (getReg rs rs1) imm,
      incPC pc
    )
runInstruction (m, rs, pc) ORI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.or (getReg rs rs1) imm,
      incPC pc
    )
runInstruction (m, rs, pc) SLLI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.shl (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
      incPC pc
    )
runInstruction (m, rs, pc) SRLI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.lshr (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
      incPC pc
    )
runInstruction (m, rs, pc) SRAI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.ashr (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
      incPC pc
    )
runInstruction (m, rs, pc) ADD [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.add (knownNat @Size) (getReg rs rs1) (getReg rs rs2),
      incPC pc
    )
runInstruction (m, rs, pc) SUB [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.sub (knownNat @Size) (getReg rs rs1) (getReg rs rs2),
      incPC pc
    )
runInstruction (m, rs, pc) SLL [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        BV.shl
          (knownNat @Size)
          (getReg rs rs1)
          (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
      incPC pc
    )
runInstruction (m, rs, pc) SLT [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
          then one
          else zero,
      incPC pc
    )
runInstruction (m, rs, pc) SLTU [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ if BV.ult (getReg rs rs1) (getReg rs rs2) then one else zero,
      incPC pc
    )
runInstruction (m, rs, pc) XOR [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.xor (getReg rs rs1) (getReg rs rs2),
      incPC pc
    )
runInstruction (m, rs, pc) SRL [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        BV.lshr
          (knownNat @Size)
          (getReg rs rs1)
          (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
      incPC pc
    )
runInstruction (m, rs, pc) SRA [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        BV.ashr
          (knownNat @Size)
          (getReg rs rs1)
          (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
      incPC pc
    )
runInstruction (m, rs, pc) AND [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.and (getReg rs rs1) (getReg rs rs2),
      incPC pc
    )
runInstruction (m, rs, pc) OR [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.or (getReg rs rs1) (getReg rs rs2),
      incPC pc
    )
runInstruction _ FENCE _ _ = undefined
runInstruction s ECALL [] [] = runCall s
runInstruction _ EBREAK _ _ = undefined
runInstruction _ _ _ _ = undefined

readNextInstruction :: State -> Either Text (Instruction, [Register], [Immediate])
readNextInstruction (m, _, pc) =
  decode $
    viewSomeBV (knownNat @Size) $
      getMem (bytestringLE . toStrict) m 4 pc

run :: State -> IO [State]
run =
  unfoldrM
    ( \ps ->
        case readNextInstruction ps of
          Left _ -> return Nothing
          Right (i, reg, imm) -> do
            ns <- runInstruction ps i reg imm
            return $ Just (ps, ns)
    )
