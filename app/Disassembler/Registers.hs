module Disassembler.Registers where

import Data.BitVector.Sized
import Registers

decodeRegister :: BV 5 -> Register
decodeRegister (BV 0) = ZERO
decodeRegister (BV 1) = RA
decodeRegister (BV 2) = SP
decodeRegister (BV 3) = GP
decodeRegister (BV 4) = TP
decodeRegister (BV 5) = T0
decodeRegister (BV 6) = T1
decodeRegister (BV 7) = T2
decodeRegister (BV 8) = S0
decodeRegister (BV 9) = S1
decodeRegister (BV 10) = A0
decodeRegister (BV 11) = A1
decodeRegister (BV 12) = A2
decodeRegister (BV 13) = A3
decodeRegister (BV 14) = A4
decodeRegister (BV 15) = A5
decodeRegister (BV 16) = A6
decodeRegister (BV 17) = A7
decodeRegister (BV 18) = S2
decodeRegister (BV 19) = S3
decodeRegister (BV 20) = S4
decodeRegister (BV 21) = S5
decodeRegister (BV 22) = S6
decodeRegister (BV 23) = S7
decodeRegister (BV 24) = S8
decodeRegister (BV 25) = S9
decodeRegister (BV 26) = S10
decodeRegister (BV 27) = S11
decodeRegister (BV 28) = T3
decodeRegister (BV 29) = T4
decodeRegister (BV 30) = T5
decodeRegister (BV 31) = T6
decodeRegister r = error $ "Invalid register: " <> ppBin (knownNat @5) r
