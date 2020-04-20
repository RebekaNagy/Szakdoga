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

helpCalculate :: String -> String -> String
helpCalculate program conditions = envListToString (verifyP4 (fst (fst prepared)) (snd prepared) sidecons)
    where { prepared = (mainConversion (parseString program) ((initEnv, finalEnv), (initActions, initTables, initProg)));
        sidecons = sideConditionConversion conditions
        }

cCalculate :: CWString -> CWString -> CWString
cCalculate program conditions = unsafePerformIO $ withCWString result $ (\newCStr -> return newCStr)
    where result = helpCalculate (unsafePerformIO (peekCWString program)) (unsafePerformIO (peekCWString conditions))

foreign export ccall cCalculate :: CWString -> CWString -> CWString