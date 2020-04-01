{-# LANGUAGE ForeignFunctionInterface #-}

module String () where
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

cstring :: CWString -> CWString
cstring cstr = unsafePerformIO $ withCWString (unsafePerformIO (peekCWString cstr)) $ (\newCStr -> return newCStr)
--    str <- peekCString cstr
--    cResult <- withCString "hellooooooooooo" $ (\newCStr -> return newCStr)
--    return cResult

foreign export ccall cstring :: CWString -> CWString