module Instructions where

import Data.BitVector.Sized

type Size = 32

size :: Integer
size = 32

type Immediate = BV Size

data Instruction
  = LUI
  | AUIPC
  | JAL
  | JALR
  | BEQ
  | BNE
  | BLT
  | BGE
  | BLTU
  | BGEU
  | LB
  | LH
  | LW
  | LBU
  | LHU
  | SB
  | SH
  | SW
  | ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  | SLLI
  | SRLI
  | SRAI
  | ADD
  | SUB
  | SLL
  | SLT
  | SLTU
  | XOR
  | SRL
  | SRA
  | OR
  | AND
  | FENCE
  | ECALL
  | EBREAK
  deriving (Show)
