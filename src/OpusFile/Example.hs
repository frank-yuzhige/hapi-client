{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OpusFile.Example where
import Test.HAPI
import Foreign.C
import Data.Data (Typeable)

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Foreign
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import GHC.Ptr (Ptr(..))

import qualified Test.HAPI.HLib.HLibPrelude as HLib
import qualified Test.HAPI.HLib.HLibPtr     as HLib
import qualified Test.HAPI.HLib.HLibCString as HLib
import qualified Test.HAPI.HLib.HLibFS      as HLib

import Test.HAPI.HLib.HLibPrelude (HLibPrelude)
import Test.HAPI.HLib.HLibPtr (HLibPtr)
import Test.HAPI.HLib.HLibCString (HLibCString)
import Test.HAPI.HLib.HLibFS (HLibFS)

conduct :: LibFuzzerConduct
conduct = libFuzzerConductViaAASTG gOpenMemory

foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
  :: CString -> CSize -> IO CInt

testOneInputM = llvmFuzzerTestOneInputM conduct

main = mainM conduct

data OggOpusFile

foreign import ccall "op_free"
  op_free :: Ptr OggOpusFile -> IO ()

foreign import ccall "op_test_memory"
  op_test_memory :: Ptr CChar -> CInt -> Ptr CInt -> IO (Ptr OggOpusFile)

foreign import ccall "op_test_open"
  op_test_open :: Ptr OggOpusFile -> IO CInt

foreign import ccall "op_channel_count"
  op_channel_count :: Ptr OggOpusFile -> CInt -> IO CInt

foreign import ccall "op_pcm_total"
  op_pcm_total :: Ptr OggOpusFile -> CInt -> IO Int64

foreign import ccall "op_read"
  op_read
    :: Ptr OggOpusFile -- _of
    -> Ptr Int16       -- _pcm
    -> CInt            -- _buf_size
    -> Ptr CInt        -- _li
    -> IO CInt

data OpusFileApi :: ApiDefinition where
  TestMemory   :: OpusFileApi '[Ptr CChar, CInt, Ptr CInt] (Ptr OggOpusFile)
  TestOpen     :: OpusFileApi '[Ptr OggOpusFile] CInt
  Free         :: OpusFileApi '[Ptr OggOpusFile] ()
  ChannelCount :: OpusFileApi '[Ptr OggOpusFile, CInt] CInt
  PcmTotal     :: OpusFileApi '[Ptr OggOpusFile, CInt] Int64
  Read         :: OpusFileApi '[Ptr OggOpusFile, Ptr Int16, CInt, Ptr CInt] CInt


deriving instance Typeable (OpusFileApi p a)
deriving instance Show     (OpusFileApi p a)
deriving instance Eq       (OpusFileApi p a)

instance ApiName      OpusFileApi where
  apiNameUnder "C" = \case
    TestMemory   -> "op_test_memory"
    TestOpen     -> "op_test_open"
    Free         -> "op_free"
    ChannelCount -> "op_channel_count"
    PcmTotal     -> "op_pcm_total"
    Read         -> "op_read"
  apiNameUnder _ = apiName

instance Entry2BlockC OpusFileApi

instance HasForeignDef OpusFileApi where
  evalForeign = \case
    TestMemory   -> implE $ \p x q -> liftIO $ op_test_memory p x q
    TestOpen     -> implE $ liftIO . op_test_open
    Free         -> implE $ liftIO . op_free
    ChannelCount -> implE $ \p x -> liftIO $ op_channel_count p x
    PcmTotal     -> implE $ \p x -> liftIO $ op_pcm_total p x
    Read         -> implE $ \f p b l -> liftIO $ op_read f p b l

type A = OpusFileApi :$$: HLibPrelude :$$: HLibPtr :$$: HLibCString :$$: HLibFS

type C = Fuzzable :<>: HSerialize :<>: CCodeGen

gOpenMemory :: AASTG A C
gOpenMemory = runEnv $ runBuildAASTG $ do
  ctnt  <- p <%> var anything
  path  <- p <%> call HLib.NewFile (getVar ctnt)
  cp    <- p <%> call HLib.NewCString (getVar path)
  l'    <- p <%> call HLib.len (getVar path)
  l     <- p <%> call HLib.fromIntegral (getVar l')
  eptr  <- p <%> call (HLib.Malloc @CInt) ()
  file  <- p <%> call TestMemory (getVar cp, getVar l, getVar eptr)
  p <%> ifFalse HLib.IsNullPtr (getVar file)
  r     <- p <%> call TestOpen (getVar file)
  p <%> assertTrue (HLib.==) (getVar r, value 0)
  p <%> call Free (getVar file)
  return ()
  where p = Building @A @C

instance TyConstC OggOpusFile where
  toCConst _ = undefined -- Phantom type
  toCType  _ = ctype "OggOpusFile"
