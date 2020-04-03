{-# LANGUAGE ForeignFunctionInterface #-}

module Calculation () where
import Verification
import Parser
import Preparation
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
import Control.Monad.IO.Class

helpCalculate :: String -> String
helpCalculate str =
    programToString (snd (mainConversion (parseString str) ((initEnv, finalEnv), (initActions, initTables, initProg))))

cCalculate :: CWString -> CWString
cCalculate cStr = unsafePerformIO $ withCWString result $ (\newCStr -> return newCStr)
    where result = helpCalculate (unsafePerformIO (peekCWString cStr))

foreign export ccall cCalculate :: CWString -> CWString