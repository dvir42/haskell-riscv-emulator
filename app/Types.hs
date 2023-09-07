module Types where

import Data.Int (Int64)

data Endianness = BE | LE deriving (Show, Eq)

data Architecture = A32 | A64 deriving (Show, Eq)

architectureToInt :: Architecture -> Int64
architectureToInt A32 = 32
architectureToInt A64 = 64
