{-# LANGUAGE ForeignFunctionInterface #-}

module Calculation () where
import Verification
import Parser
import Preparation
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.IO.Class

helpCalculate :: String -> String
helpCalculate str =
    programToString (snd (mainConversion (parseString str) ((initEnv, finalEnv), (initActions, initTables, initProg))))

cCalculate :: CString -> IO CString
cCalculate cStr = do
    str <- peekCString cStr
    let result = helpCalculate str
    cResult <- newCString result
    return cResult

foreign export ccall cCalculate :: CString -> IO CString