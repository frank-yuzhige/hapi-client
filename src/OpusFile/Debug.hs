module OpusFile.Debug where

import OpusFile.Example
import Test.HAPI (LibFuzzerConduct(llvmFuzzerDebugM))
import Foreign.C

foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
  :: CString -> CSize -> IO CInt

testOneInputM = llvmFuzzerDebugM conduct
