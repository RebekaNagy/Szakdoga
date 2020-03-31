{-# LANGUAGE ForeignFunctionInterface #-}

module Fibonacci () where
import Calculation
import Data.Word
import Foreign.C.Types

c_fibonacci :: CUChar -> CUInt
c_fibonacci (CUChar n) = CUInt (fibonacci n)

foreign export ccall c_fibonacci :: CUChar -> CUInt