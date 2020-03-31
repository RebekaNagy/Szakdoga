{-# LANGUAGE ForeignFunctionInterface #-}

module Calculation () where
import Verification
import Parser
import Preparation
import Data.Word
import Foreign.C.Types

fibs :: [Word32]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fibonacci :: Word8 -> Word32
fibonacci n =
  if n > 47
    then 0
    else fibs !! (fromIntegral n)

c_fibonacci :: CUChar -> CUInt
c_fibonacci (CUChar n) = CUInt (fibonacci n)

foreign export ccall c_fibonacci :: CUChar -> CUInt
