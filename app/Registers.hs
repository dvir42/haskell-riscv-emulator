module Registers where

data Register
  = X0  | ZERO
  | X1  | RA
  | X2  | SP
  | X3  | GP
  | X4  | TP
  | X5  | T0
  | X6  | T1
  | X7  | T2
  | X8  | S0 | FP
  | X9  | S1
  | X10 | A0
  | X11 | A1
  | X12 | A2
  | X13 | A3
  | X14 | A4
  | X15 | A5
  | X16 | A6
  | X17 | A7
  | X18 | S2
  | X19 | S3
  | X20 | S4
  | X21 | S5
  | X22 | S6
  | X23 | S7
  | X24 | S8
  | X25 | S9
  | X26 | S10
  | X27 | S11
  | X28 | T3
  | X29 | T4
  | X30 | T5
  | X31 | T6
  deriving (Show, Eq, Ord)

