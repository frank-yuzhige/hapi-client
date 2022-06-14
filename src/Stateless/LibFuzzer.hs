module Stateless.LibFuzzer where
import Test.HAPI.Conduct (LibFuzzerConduct, libFuzzerConductViaAASTG, llvmFuzzerTestOneInputM, mainM)
import Foreign.C
import Stateless.Example (cograph)

conduct :: LibFuzzerConduct
conduct = libFuzzerConductViaAASTG [] cograph

foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
  :: CString -> CSize -> IO CInt

testOneInputM = llvmFuzzerTestOneInputM conduct

main = mainM conduct
