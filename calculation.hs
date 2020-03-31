module Calculation where
import Data.Word

fibs :: [Word32]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fibonacci :: Word8 -> Word32
fibonacci n =
  if n > 47
    then 0
    else fibs !! (fromIntegral n)