module OpusFile.Test where
import Foreign.C
import OpusFile.Example (conduct)
import Test.HAPI

foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
  :: CString -> CSize -> IO CInt

testOneInputM = llvmFuzzerTestOneInputM conduct
