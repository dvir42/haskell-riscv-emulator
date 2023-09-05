module Disassembler.Instructions
  ( decodeInstruction,
    Instruction (..),
    InstructionType (..),
    instructionType,
    OpcodeType (..),
  )
where

import Data.BitVector.Sized
import qualified Data.BitVector.Sized as BV
import Data.Text (Text, pack)
import Instructions

data OpcodeType
  = TYPE_LOAD
  | TYPE_STORE
  | TYPE_MADD
  | TYPE_BRANCH
  | TYPE_LOAD_FP
  | TYPE_STORE_FP
  | TYPE_MSUB
  | TYPE_JALR
  | TYPE_CUSTOM_0
  | TYPE_CUSTOM_1
  | TYPE_NMSUB
  | TYPE_RESERVED
  | TYPE_MISC_MEM
  | TYPE_AMO
  | TYPE_NMADD
  | TYPE_JAL
  | TYPE_OP_IMM
  | TYPE_OP
  | TYPE_OP_FP
  | TYPE_SYSTEM
  | TYPE_AUIPC
  | TYPE_LUI
  | TYPE_OP_IMM_32
  | TYPE_OP_32
  | TYPE_CUSTOM_2
  | TYPE_CUSTOM_3
  | TYPE_48b
  | TYPE_64b
  | TYPE_80b
  deriving (Show)

opcodeType :: BV Size -> Either Text OpcodeType
opcodeType ins
  | match 0b0000011 = Right TYPE_LOAD
  | match 0b0100011 = Right TYPE_STORE
  | match 0b1000011 = Right TYPE_MADD
  | match 0b1100011 = Right TYPE_BRANCH
  | match 0b0000111 = Right TYPE_LOAD_FP
  | match 0b0100111 = Right TYPE_STORE_FP
  | match 0b1000111 = Right TYPE_MSUB
  | match 0b1100111 = Right TYPE_JALR
  | match 0b0001011 = Right TYPE_CUSTOM_0
  | match 0b0101011 = Right TYPE_CUSTOM_1
  | match 0b1001011 = Right TYPE_NMSUB
  | match 0b1101011 = Right TYPE_RESERVED
  | match 0b0001111 = Right TYPE_MISC_MEM
  | match 0b0101111 = Right TYPE_AMO
  | match 0b1001111 = Right TYPE_NMADD
  | match 0b1101111 = Right TYPE_JAL
  | match 0b0010011 = Right TYPE_OP_IMM
  | match 0b0110011 = Right TYPE_OP
  | match 0b1010011 = Right TYPE_OP_FP
  | match 0b1110011 = Right TYPE_SYSTEM
  | match 0b0010111 = Right TYPE_AUIPC
  | match 0b0110111 = Right TYPE_LUI
  | match 0b1010111 = Right TYPE_RESERVED
  | match 0b1110111 = Right TYPE_RESERVED
  | match 0b0011011 = Right TYPE_OP_IMM_32
  | match 0b0111011 = Right TYPE_OP_32
  | match 0b1011011 = Right TYPE_CUSTOM_2
  | match 0b1111011 = Right TYPE_CUSTOM_3
  | match 0b0011111 = Right TYPE_48b
  | match 0b0111111 = Right TYPE_64b
  | match 0b1011111 = Right TYPE_48b
  | match 0b1111111 = Right TYPE_80b
  | otherwise = Left $ "Illegal instruction type: " <> pack (ppBin (knownNat @32) ins)
  where
    match bits =
      xor
        (BV.and (mkBV (knownNat @Size) 0b1111111) ins)
        (mkBV (knownNat @Size) bits)
        == mkBV (knownNat @Size) 0

decodeTypeLoad :: BV 3 -> Either Text Instruction
decodeTypeLoad (BV 0b000) = Right LB
decodeTypeLoad (BV 0b001) = Right LH
decodeTypeLoad (BV 0b010) = Right LW
decodeTypeLoad (BV 0b100) = Right LBU
decodeTypeLoad (BV 0b101) = Right LHU
decodeTypeLoad f =
  Left $
    "Invalid "
      <> pack (show TYPE_LOAD)
      <> " inscruction. funct3: "
      <> pack (ppBin (knownNat @3) f)

decodeTypeStore :: BV 3 -> Either Text Instruction
decodeTypeStore (BV 0b000) = Right SB
decodeTypeStore (BV 0b001) = Right SH
decodeTypeStore (BV 0b010) = Right SW
decodeTypeStore f =
  Left $
    "Invalid "
      <> pack (show TYPE_STORE)
      <> " inscruction. funct3: "
      <> pack (ppBin (knownNat @3) f)

decodeTypeBranch :: BV 3 -> Either Text Instruction
decodeTypeBranch (BV 0b000) = Right BEQ
decodeTypeBranch (BV 0b001) = Right BNE
decodeTypeBranch (BV 0b100) = Right BLT
decodeTypeBranch (BV 0b101) = Right BGE
decodeTypeBranch (BV 0b110) = Right BLTU
decodeTypeBranch (BV 0b111) = Right BGEU
decodeTypeBranch f =
  Left $
    "Invalid "
      <> pack (show TYPE_BRANCH)
      <> " inscruction. funct3: "
      <> pack (ppBin (knownNat @3) f)

decodeTypeMiscMem :: BV 3 -> Either Text Instruction
decodeTypeMiscMem (BV 0b000) = Right FENCE
decodeTypeMiscMem f =
  Left $
    "Invalid "
      <> pack (show TYPE_MISC_MEM)
      <> " inscruction. funct3: "
      <> pack (ppBin (knownNat @3) f)

decodeTypeOpImm :: BV 3 -> BV 7 -> Either Text Instruction
decodeTypeOpImm (BV 0b000) _ = Right ADDI
decodeTypeOpImm (BV 0b010) _ = Right SLTI
decodeTypeOpImm (BV 0b011) _ = Right SLTIU
decodeTypeOpImm (BV 0b100) _ = Right XORI
decodeTypeOpImm (BV 0b110) _ = Right ORI
decodeTypeOpImm (BV 0b111) _ = Right ANDI
decodeTypeOpImm (BV 0b001) (BV 0b0000000) = Right SLLI
decodeTypeOpImm (BV 0b101) (BV 0b0000000) = Right SRLI
decodeTypeOpImm (BV 0b101) (BV 0b0100000) = Right SRAI
decodeTypeOpImm f3 f7 =
  Left $
    "Invalid "
      <> pack (show TYPE_OP_IMM)
      <> " instruction. funct3: "
      <> pack (ppBin (knownNat @3) f3)
      <> ", funct7: "
      <> pack (ppBin (knownNat @7) f7)

decodeTypeOp :: BV 3 -> BV 7 -> Either Text Instruction
decodeTypeOp (BV 0b000) (BV 0b0000000) = Right ADD
decodeTypeOp (BV 0b000) (BV 0b0100000) = Right SUB
decodeTypeOp (BV 0b001) (BV 0b0000000) = Right SLL
decodeTypeOp (BV 0b010) (BV 0b0000000) = Right SLT
decodeTypeOp (BV 0b011) (BV 0b0000000) = Right SLTU
decodeTypeOp (BV 0b100) (BV 0b0000000) = Right XOR
decodeTypeOp (BV 0b101) (BV 0b0000000) = Right SRL
decodeTypeOp (BV 0b101) (BV 0b0100000) = Right SRA
decodeTypeOp (BV 0b110) (BV 0b0000000) = Right OR
decodeTypeOp (BV 0b111) (BV 0b0000000) = Right AND
decodeTypeOp f3 f7 =
  Left $
    "Invalid "
      <> pack (show TYPE_OP)
      <> " instruction. funct3: "
      <> pack (ppBin (knownNat @3) f3)
      <> ", funct7: "
      <> pack (ppBin (knownNat @7) f7)

decodeTypeSystem :: BV 25 -> Either Text Instruction
decodeTypeSystem (BV 0b0000000000000000000000000) = Right ECALL
decodeTypeSystem (BV 0b0000000000010000000000000) = Right EBREAK
decodeTypeSystem f =
  Left $
    "Invalid "
      <> pack (show TYPE_SYSTEM)
      <> " instruction: "
      <> pack (ppBin (knownNat @25) f)

funct3 :: BV Size -> BV 3
funct3 = select (knownNat @12) (knownNat @3)

funct7 :: BV Size -> BV 7
funct7 = select (knownNat @25) (knownNat @7)

decodeInstruction :: BV Size -> Either Text Instruction
decodeInstruction ins = case opcodeType ins of
  Right TYPE_LOAD -> decodeTypeLoad $ funct3 ins
  Right TYPE_STORE -> decodeTypeStore $ funct3 ins
  Right TYPE_BRANCH -> decodeTypeBranch $ funct3 ins
  Right TYPE_JALR -> Right JALR
  Right TYPE_MISC_MEM -> decodeTypeMiscMem $ funct3 ins
  Right TYPE_JAL -> Right JAL
  Right TYPE_OP_IMM -> decodeTypeOpImm (funct3 ins) (funct7 ins)
  Right TYPE_OP -> decodeTypeOp (funct3 ins) (funct7 ins)
  Right TYPE_SYSTEM -> decodeTypeSystem $ select (knownNat @7) (knownNat @25) ins
  Right TYPE_AUIPC -> Right AUIPC
  Right TYPE_LUI -> Right LUI
  Left e -> Left e
  t -> Left $ "Unsupported instruction type: " <> pack (show t)

data InstructionType
  = R
  | I
  | I_EXT
  | S
  | B
  | U
  | J
  | FENCE_TYPE
  | SYSTEM_TYPE
  deriving (Show)

instructionType :: Instruction -> InstructionType
instructionType LUI = U
instructionType AUIPC = U
instructionType JAL = J
instructionType JALR = I
instructionType BEQ = B
instructionType BNE = B
instructionType BLT = B
instructionType BGE = B
instructionType BLTU = B
instructionType BGEU = B
instructionType LB = I
instructionType LH = I
instructionType LW = I
instructionType LBU = I
instructionType LHU = I
instructionType SB = S
instructionType SH = S
instructionType SW = S
instructionType ADDI = I
instructionType SLTI = I
instructionType SLTIU = I
instructionType XORI = I
instructionType ORI = I
instructionType ANDI = I
instructionType SLLI = I_EXT
instructionType SRLI = I_EXT
instructionType SRAI = I_EXT
instructionType ADD = R
instructionType SUB = R
instructionType SLL = R
instructionType SLT = R
instructionType SLTU = R
instructionType XOR = R
instructionType SRL = R
instructionType SRA = R
instructionType OR = R
instructionType AND = R
instructionType FENCE = FENCE_TYPE
instructionType ECALL = SYSTEM_TYPE
instructionType EBREAK = SYSTEM_TYPE
