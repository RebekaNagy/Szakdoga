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
helpCalculate program conditions =    
    case (fst(fst prepared)) of
        [("", Stuck, EnvError)] -> "A parser vagy deparser helytelen, így nem sikerült a környezetek megfelelő generálása.&" 
        _ -> case (snd prepared) of
            EmptyProg -> "A program nem tartalmaz verifikálásra alkalmas részt.&"
            ProgError -> "A program szintaktikailag helytelen, vagy a vizsgált résznyelven kívül esik.&"
            _ -> case sidecons of
                SideCondError -> "Hiba a mellékfeltételek generálásakor, vagy átadásakor.&"
                _ -> "NOERROR&" ++ envListToString (compareCalculatedWithFinal (verifyP4 (fst (fst prepared)) (snd prepared) sidecons 0) (snd (fst prepared))) ++ "&" ++ (show (snd (fst prepared))) ++ "&" ++ (show (fst (fst prepared)))
    where { prepared = (mainConversion (parseString program) ((initEnv, finalEnv), (initActions, initTables, initProg)));
        sidecons = sideConditionConversion conditions
        }

cCalculate :: CWString -> CWString -> CWString
cCalculate program conditions = unsafePerformIO $ withCWString result $ (\newCStr -> return newCStr)
    where result = helpCalculate (unsafePerformIO (peekCWString program)) (unsafePerformIO (peekCWString conditions))

foreign export ccall cCalculate :: CWString -> CWString -> CWString