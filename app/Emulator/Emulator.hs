module Emulator.Emulator where

import Control.Monad.Loops (unfoldrM)
import Data.BitVector.Sized (asBytesBE, asBytesLE, asUnsigned, bytestringBE, bytestringLE, knownNat, truncBits)
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
runInstruction (m, rs, pc, p) LUI [rd] [imm] =
  return
    ( m,
      setReg rs rd imm,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) AUIPC [rd] [imm] =
  return
    ( m,
      setReg rs rd $ BV.add (knownNat @Size) pc imm,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) JAL [rd] [imm] =
  return
    ( m,
      setReg rs rd $ incPC pc,
      BV.add (knownNat @32) pc imm,
      p
    )
runInstruction (m, rs, pc, p) JALR [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ incPC pc,
      BV.add
        (knownNat @Size)
        pc
        $ BV.clearBit (knownNat @Size) (knownNat @0)
        $ BV.add (knownNat @Size) imm
        $ getReg rs rs1,
      p
    )
runInstruction (m, rs, pc, p) BEQ [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if getReg rs rs1 == getReg rs rs2
        then BV.add (knownNat @Size) imm pc
        else incPC pc,
      p
    )
runInstruction (m, rs, pc, p) BNE [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if getReg rs rs1 /= getReg rs rs2
        then BV.add (knownNat @Size) imm pc
        else incPC pc,
      p
    )
runInstruction (m, rs, pc, p) BLT [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc,
      p
    )
runInstruction (m, rs, pc, p) BLT [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc,
      p
    )
runInstruction (m, rs, pc, p) BGE [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if not $ BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc,
      p
    )
runInstruction (m, rs, pc, p) BLTU [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if BV.ult (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc,
      p
    )
runInstruction (m, rs, pc, p) BGEU [rs1, rs2] [imm] =
  return
    ( m,
      rs,
      if not $ BV.ult (getReg rs rs1) (getReg rs rs2)
        then BV.add (knownNat @Size) imm pc
        else incPC pc,
      p
    )
runInstruction (m, rs, pc, p) LB [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.sext (knownNat @8) (knownNat @Size) $
          viewSomeBV (knownNat @8) $
            getMem (bytestringFunc . toStrict) m 1 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc,
      p
    )
  where
    bytestringFunc = case endianness p of
      BE -> bytestringBE
      LE -> bytestringLE
runInstruction (m, rs, pc, p) LH [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.sext (knownNat @16) (knownNat @Size) $
          viewSomeBV (knownNat @16) $
            getMem (bytestringFunc . toStrict) m 2 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc,
      p
    )
  where
    bytestringFunc = case endianness p of
      BE -> bytestringBE
      LE -> bytestringLE
runInstruction (m, rs, pc, p) LW [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        viewSomeBV (knownNat @Size) $
          getMem (bytestringFunc . toStrict) m 4 $
            BV.add (knownNat @Size) imm $
              getReg rs rs1,
      incPC pc,
      p
    )
  where
    bytestringFunc = case endianness p of
      BE -> bytestringBE
      LE -> bytestringLE
runInstruction (m, rs, pc, p) LBU [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.zext (knownNat @Size) $
          viewSomeBV (knownNat @8) $
            getMem (bytestringFunc . toStrict) m 1 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc,
      p
    )
  where
    bytestringFunc = case endianness p of
      BE -> bytestringBE
      LE -> bytestringLE
runInstruction (m, rs, pc, p) LHU [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.zext (knownNat @Size) $
          viewSomeBV (knownNat @16) $
            getMem (bytestringFunc . toStrict) m 2 $
              BV.add (knownNat @Size) imm $
                getReg rs rs1,
      incPC pc,
      p
    )
  where
    bytestringFunc = case endianness p of
      BE -> bytestringBE
      LE -> bytestringLE
runInstruction (m, rs, pc, p) SB [rs1, rs2] [imm] =
  return
    ( setMem
        m
        (BV.add (knownNat @Size) imm $ getReg rs rs1)
        ( fromMaybe undefined $
            asBytesFunc (knownNat @8) $
              BV.trunc (knownNat @8) $
                getReg rs rs2
        ),
      rs,
      incPC pc,
      p
    )
  where
    asBytesFunc = case endianness p of
      BE -> asBytesBE
      LE -> asBytesLE
runInstruction (m, rs, pc, p) SH [rs1, rs2] [imm] =
  return
    ( setMem
        m
        (BV.add (knownNat @Size) imm $ getReg rs rs1)
        ( fromMaybe undefined $
            asBytesFunc (knownNat @16) $
              BV.trunc (knownNat @16) $
                getReg rs rs2
        ),
      rs,
      incPC pc,
      p
    )
  where
    asBytesFunc = case endianness p of
      BE -> asBytesBE
      LE -> asBytesLE
runInstruction (m, rs, pc, p) SW [rs1, rs2] [imm] =
  return
    ( setMem
        m
        (BV.add (knownNat @Size) imm $ getReg rs rs1)
        ( fromMaybe undefined $
            asBytesFunc (knownNat @Size) $
              getReg rs rs2
        ),
      rs,
      incPC pc,
      p
    )
  where
    asBytesFunc = case endianness p of
      BE -> asBytesBE
      LE -> asBytesLE
runInstruction (m, rs, pc, p) ADDI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.add (knownNat @Size) (getReg rs rs1) imm,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SLTI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ if BV.slt (knownNat @Size) (getReg rs rs1) imm then one else zero,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SLTIU [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ if BV.ult (getReg rs rs1) imm then one else zero,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) XORI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.xor (getReg rs rs1) imm,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) ANDI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.and (getReg rs rs1) imm,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) ORI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $ BV.or (getReg rs rs1) imm,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SLLI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.shl (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SRLI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.lshr (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SRAI [rd, rs1] [imm] =
  return
    ( m,
      setReg rs rd $
        BV.ashr (knownNat @Size) (getReg rs rs1) (fromIntegral $ asUnsigned imm),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) ADD [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.add (knownNat @Size) (getReg rs rs1) (getReg rs rs2),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SUB [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.sub (knownNat @Size) (getReg rs rs1) (getReg rs rs2),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SLL [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        BV.shl
          (knownNat @Size)
          (getReg rs rs1)
          (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SLT [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        if BV.slt (knownNat @Size) (getReg rs rs1) (getReg rs rs2)
          then one
          else zero,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SLTU [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ if BV.ult (getReg rs rs1) (getReg rs rs2) then one else zero,
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) XOR [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.xor (getReg rs rs1) (getReg rs rs2),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SRL [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        BV.lshr
          (knownNat @Size)
          (getReg rs rs1)
          (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) SRA [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $
        BV.ashr
          (knownNat @Size)
          (getReg rs rs1)
          (fromIntegral $ asUnsigned $ truncBits 5 $ getReg rs rs2),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) AND [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.and (getReg rs rs1) (getReg rs rs2),
      incPC pc,
      p
    )
runInstruction (m, rs, pc, p) OR [rd, rs1, rs2] [] =
  return
    ( m,
      setReg rs rd $ BV.or (getReg rs rs1) (getReg rs rs2),
      incPC pc,
      p
    )
runInstruction _ FENCE _ _ = undefined
runInstruction s ECALL [] [] = runCall s
runInstruction _ EBREAK _ _ = undefined
runInstruction _ _ _ _ = undefined

readNextInstruction :: State -> Either Text (Instruction, [Register], [Immediate])
readNextInstruction (m, _, pc, p) =
  decode $
    viewSomeBV (knownNat @Size) $
      getMem (bytestringFunc . toStrict) m 4 pc
  where
    bytestringFunc = case endianness p of
      BE -> bytestringBE
      LE -> bytestringLE

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
