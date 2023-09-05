module Disassembler.Disassembler (decode) where

import Data.BitVector.Sized
import qualified Data.BitVector.Sized as BV
import Data.Proxy
import Data.Text (Text, pack, toLower)
import Disassembler.Instructions
import Disassembler.Registers
import GHC.TypeNats (natVal)
import Instructions
import Registers

rd :: BV Size -> Register
rd ins = decodeRegister $ select (knownNat @7) (knownNat @5) ins

rs1 :: BV Size -> Register
rs1 ins = decodeRegister $ select (knownNat @15) (knownNat @5) ins

rs2 :: BV Size -> Register
rs2 ins = decodeRegister $ select (knownNat @20) (knownNat @5) ins

decodeRType :: BV Size -> ([Register], [BV Size])
decodeRType ins = ([rd ins, rs1 ins, rs2 ins], [])

decodeIType :: BV Size -> ([Register], [BV Size])
decodeIType ins =
  ( [rd ins, rs1 ins],
    [ sext (knownNat @12) (knownNat @Size) $
        select (knownNat @20) (knownNat @12) ins
    ]
  )

decodeIExtType :: BV Size -> ([Register], [BV Size])
decodeIExtType ins =
  ( [rd ins, rs1 ins],
    [ sext (knownNat @5) (knownNat @Size) $
        select (knownNat @20) (knownNat @5) ins
    ]
  )

decodeSType :: BV Size -> ([Register], [BV Size])
decodeSType ins =
  ( [rs1 ins, rs2 ins],
    [ sext (knownNat @12) (knownNat @Size) $
        BV.concat
          (knownNat @7)
          (knownNat @5)
          (select (knownNat @25) (knownNat @7) ins)
          (select (knownNat @7) (knownNat @5) ins)
    ]
  )

decodeBType :: BV Size -> ([Register], [BV Size])
decodeBType ins =
  ( [rs1 ins, rs2 ins],
    [ sext (knownNat @13) (knownNat @Size) $
        rotateL
          (knownNat @13)
          ( zext (knownNat @13) $
              BV.concat
                (knownNat @2)
                (knownNat @10)
                ( BV.concat
                    (knownNat @1)
                    (knownNat @1)
                    (select (knownNat @31) (knownNat @1) ins)
                    (select (knownNat @7) (knownNat @1) ins)
                )
                ( BV.concat
                    (knownNat @6)
                    (knownNat @4)
                    (select (knownNat @25) (knownNat @6) ins)
                    (select (knownNat @8) (knownNat @4) ins)
                )
          )
          1
    ]
  )

decodeUType :: BV Size -> ([Register], [BV Size])
decodeUType ins =
  ( [rd ins],
    [ rotateL
        (knownNat @Size)
        (zext (knownNat @Size) $ select (knownNat @12) (knownNat @20) ins)
        (natVal (Proxy @Size) - 20)
    ]
  )

decodeJType :: BV Size -> ([Register], [BV Size])
decodeJType ins =
  ( [rd ins],
    [ sext (knownNat @21) (knownNat @Size) $
        rotateL
          (knownNat @21)
          ( zext (knownNat @21) $
              BV.concat
                (knownNat @9)
                (knownNat @11)
                ( BV.concat
                    (knownNat @1)
                    (knownNat @8)
                    (select (knownNat @31) (knownNat @1) ins)
                    (select (knownNat @12) (knownNat @8) ins)
                )
                ( BV.concat
                    (knownNat @1)
                    (knownNat @10)
                    (select (knownNat @20) (knownNat @1) ins)
                    (select (knownNat @21) (knownNat @10) ins)
                )
          )
          1
    ]
  )

decode :: BV Size -> Either Text (Instruction, [Register], [BV Size])
decode ins = case decodeInstruction ins of
  Left e -> Left e
  Right i ->
    Right $
      let args = case instructionType i of
            R -> decodeRType ins
            I -> decodeIType ins
            I_EXT -> decodeIExtType ins
            S -> decodeSType ins
            B -> decodeBType ins
            U -> decodeUType ins
            J -> decodeJType ins
            FENCE_TYPE -> undefined
            SYSTEM_TYPE -> ([], [])
       in (i, fst args, snd args)
