{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpusFile.Example where
import Test.HAPI
import Foreign
import Foreign.C
import Foreign.CStorable (CStorable (..))

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Data (Typeable)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)

import qualified Test.HAPI.HLib.HLibPrelude as HLib
import qualified Test.HAPI.HLib.HLibPtr     as HLib
import qualified Test.HAPI.HLib.HLibCString as HLib
import qualified Test.HAPI.HLib.HLibFS      as HLib

import Test.HAPI.HLib.HLibPrelude (HLibPrelude)
import Test.HAPI.HLib.HLibPtr     (HLibPtr)
import Test.HAPI.HLib.HLibCString (HLibCString)
import Test.HAPI.HLib.HLibFS      (HLibFS)

conduct :: LibFuzzerConduct
conduct = libFuzzerConductViaAASTG ["opusfile"] (castAASTG graph)
  where
    graph :: TypedAASTG A C
    graph = runEnv $ do
      gs <- runBuildTypedAASTG @A @C (gOpenMem >>= gTestOpen)
        <:> runBuildTypedAASTG @A @C gTagsParse
        <:> runBuildTypedAASTG @A @C gTagsParseNull
        <:> runBuildTypedAASTG @A @C (gOpenMem >>= gDecodeInt16 >>= gFree)
        <:> runBuildTypedAASTG @A @C (gOpenFile >>= gDecodeInt16 >>= gFree)
        <:> runBuildTypedAASTG @A @C (gOpenMem >>= gOpusFileFuzz)
        <:> runBuildTypedAASTG @A @C (gOpenFile >>= gOpusFileFuzz)
        -- <:> runBuildTypedAASTG @A @C (gOpenMem >> useless1)
        -- <:> runBuildTypedAASTG @A @C useless1
        -- <:> runBuildTypedAASTG @A @C useless2
        -- <:> runBuildTypedAASTG @A @C useless3
        -- <:> runBuildTypedAASTG @A @C useless4
        <:> pure []
      coalesceRuleAASTGs 500 gs

data OggOpusFile

data OpusTags = OpusTags
  { user_comments   :: Ptr (Ptr CChar)
  , comment_lengths :: Ptr CInt
  , comments        :: CInt
  , vendor          :: Ptr CChar
  }
  deriving (Show, Eq, Generic)

foreign import ccall "op_free"
  op_free :: Ptr OggOpusFile -> IO ()

foreign import ccall "op_test_memory"
  op_test_memory :: Ptr CChar -> CInt -> Ptr CInt -> IO (Ptr OggOpusFile)

foreign import ccall "op_test_file"
  op_test_file :: Ptr CChar -> Ptr CInt -> IO (Ptr OggOpusFile)

foreign import ccall "op_test_open"
  op_test_open :: Ptr OggOpusFile -> IO CInt

foreign import ccall "op_channel_count"
  op_channel_count :: Ptr OggOpusFile -> CInt -> IO CInt

foreign import ccall "op_link_count"
  op_link_count :: Ptr OggOpusFile -> IO CInt

foreign import ccall "op_pcm_total"
  op_pcm_total :: Ptr OggOpusFile -> CInt -> IO Int64

foreign import ccall "op_raw_total"
  op_raw_total :: Ptr OggOpusFile -> CInt -> IO Int64

foreign import ccall "op_pcm_tell"
  op_pcm_tell :: Ptr OggOpusFile -> IO Int64

foreign import ccall "op_raw_tell"
  op_raw_tell :: Ptr OggOpusFile -> IO Int64

foreign import ccall "op_read"
  op_read
    :: Ptr OggOpusFile -- _of
    -> Ptr Int16       -- _pcm
    -> CInt            -- _buf_size
    -> Ptr CInt        -- _li
    -> IO CInt

foreign import ccall "op_read_stereo"
  op_read_stereo
    :: Ptr OggOpusFile -- _of
    -> Ptr Int16       -- _pcm
    -> CInt            -- _buf_size
    -> IO CInt

foreign import ccall "opus_tags_parse"
  opus_tags_parse
    :: Ptr OpusTags
    -> Ptr CChar
    -> CSize
    -> IO CInt

foreign import ccall "op_tags"
  opus_tags
    :: Ptr OggOpusFile
    -> CInt
    -> IO (Ptr OpusTags)

foreign import ccall "op_current_link"
  op_current_link :: Ptr OggOpusFile -> IO CInt

data OpusFileApi :: ApiDefinition where
  TestMemory   :: OpusFileApi '[Ptr CChar, CInt, Ptr CInt] (Ptr OggOpusFile)
  TestFile     :: OpusFileApi '[Ptr CChar, Ptr CInt]       (Ptr OggOpusFile)
  TestOpen     :: OpusFileApi '[Ptr OggOpusFile] CInt
  Free         :: OpusFileApi '[Ptr OggOpusFile] ()
  ChannelCount :: OpusFileApi '[Ptr OggOpusFile, CInt] CInt
  LinkCount    :: OpusFileApi '[Ptr OggOpusFile] CInt
  PcmTotal     :: OpusFileApi '[Ptr OggOpusFile, CInt] Int64
  RawTotal     :: OpusFileApi '[Ptr OggOpusFile, CInt] Int64
  PcmTell      :: OpusFileApi '[Ptr OggOpusFile] Int64
  RawTell      :: OpusFileApi '[Ptr OggOpusFile] Int64
  Read         :: OpusFileApi '[Ptr OggOpusFile, Ptr Int16, CInt, Ptr CInt] CInt
  ReadStereo   :: OpusFileApi '[Ptr OggOpusFile, Ptr Int16, CInt] CInt
  Tags         :: OpusFileApi '[Ptr OggOpusFile, CInt] (Ptr OpusTags)
  TagsParse    :: OpusFileApi '[Ptr OpusTags, Ptr CChar, CSize] CInt
  CurrentLink  :: OpusFileApi '[Ptr OggOpusFile] CInt


deriving instance Typeable (OpusFileApi p a)
deriving instance Show     (OpusFileApi p a)
deriving instance Eq       (OpusFileApi p a)

instance ApiName      OpusFileApi where
  apiName = \case
    TestMemory   -> "op_test_memory"
    TestFile     -> "op_test_file"
    TestOpen     -> "op_test_open"
    Free         -> "op_free"
    ChannelCount -> "op_channel_count"
    LinkCount    -> "op_link_count"
    PcmTotal     -> "op_pcm_total"
    RawTotal     -> "op_raw_total"
    PcmTell      -> "op_pcm_tell"
    RawTell      -> "op_raw_tell"
    Read         -> "op_read"
    ReadStereo   -> "op_read_stereo"
    Tags         -> "op_tags"
    TagsParse    -> "opus_tags_parse"
    CurrentLink  -> "op_current_link"

instance Entry2BlockC OpusFileApi

instance HasForeignDef OpusFileApi where
  evalForeign = \case
    TestMemory   -> implE $ \p x q -> liftIO $ op_test_memory p x q
    TestFile     -> implE $ \p q   -> liftIO $ op_test_file   p q
    TestOpen     -> implE $ liftIO . op_test_open
    Free         -> implE $ liftIO . op_free
    ChannelCount -> implE $ \p x -> liftIO $ op_channel_count p x
    LinkCount    -> implE $ liftIO . op_link_count
    PcmTotal     -> implE $ \p x -> liftIO $ op_pcm_total p x
    RawTotal     -> implE $ \p x -> liftIO $ op_raw_total p x
    PcmTell      -> implE $ liftIO . op_pcm_tell
    RawTell      -> implE $ liftIO . op_raw_tell
    Read         -> implE $ \f p b l -> liftIO $ op_read f p b l
    ReadStereo   -> implE $ \f p b -> liftIO $ op_read_stereo f p b
    Tags         -> implE $ \p d   -> liftIO $ opus_tags p d
    TagsParse    -> implE $ \p d s -> liftIO $ opus_tags_parse p d s
    CurrentLink  -> implE $ liftIO . op_current_link

type A = OpusFileApi :$$: HLibPrelude :$$: HLibPtr :$$: HLibCString :$$: HLibFS

type C = Fuzzable :<>: HSerialize :<>: CCodeGen

gOpenMem :: Eff (BuildAASTG A C) sig m => m (PKey (Ptr OggOpusFile))
gOpenMem = do
  ctnt  <- p <%> decl anything
  path  <- p <%> call HLib.NewFileBytes(var ctnt)
  cp    <- p <%> call HLib.NewCString(var path)
  eptr  <- p <%> call (HLib.Malloc @CInt) ()
  file  <- p <%> call TestFile(var cp, var eptr)
  p <%> ifFalse HLib.IsNullPtr(var file)
  return file
  where p = Building @A @C

gTestOpen :: Eff (BuildAASTG A C) sig m => PKey (Ptr OggOpusFile) -> m ()
gTestOpen handle = do
  r     <- p <%> call TestOpen(var handle)
  p <%> ifTrue (HLib.==) (var r, value 0)
  gChannelCount handle
  p <%> call Free(var handle)
  return ()
  where p = Building @A @C

gFree :: Eff (BuildAASTG A C) sig m => PKey (Ptr OggOpusFile) -> m ()
gFree k = do
  p <%> call Free(var k)
  return ()
  where p = Building @A @C

gOpenFile :: Eff (BuildAASTG A C) sig m => m (PKey (Ptr OggOpusFile))
gOpenFile = do
  path  <- p <%> decl (value "sample3.opus")
  cp    <- p <%> call HLib.NewCString(var path)
  eptr  <- p <%> call (HLib.Malloc @CInt) ()
  file  <- p <%> call TestFile(var cp, var eptr)
  p <%> ifFalse HLib.IsNullPtr(var file)
  r     <- p <%> call TestOpen(var file)
  p <%> ifTrue (HLib.==) (var r, value 0)
  return file
  where p = Building @A @C

gChannelCount :: Eff (BuildAASTG A C) sig m
              => PKey (Ptr OggOpusFile)
              -> m ()
gChannelCount handle = do
  c <- p <%> call ChannelCount (var handle, value (-1))
  p <%> assert ((Value 2 .== Get c) .|| (Value 3 .== Get c))
  where p = Building @A @C

gDecodeInt16 :: Eff (BuildAASTG A C) sig m
             => PKey (Ptr OggOpusFile)
             -> m (PKey (Ptr OggOpusFile))
gDecodeInt16 handle = do
  hPcmSize <- p <%> call PcmTotal (var handle, value (-1))
  chanCnt  <- p <%> call ChannelCount (var handle, value (-1))
  p <%> contIf ((Get hPcmSize .>= Value 0) .&& (Get chanCnt .>= Value 0))
  byteSize <- p <%> decl (Direct $ DCastInt $ Get hPcmSize .* DCastInt (Get chanCnt) .* sampleBytes)
  fptr     <- p <%> call (HLib.MallocBytes @Int16) (var byteSize)
  samplesDone <- p <%> val 0
  p <%> while (Get samplesDone .== Get hPcmSize) (loopBody fptr hPcmSize chanCnt samplesDone)
  return handle
  where
    sampleBytes = Value $ fromIntegral $ Foreign.sizeOf (0 :: Int16)
    p = Building @A @C
    loopBody fptr hPcmSize chanCnt samplesDone = do
      ret <- p <%> call Read (var handle, var fptr, Direct (DCastInt $ Get hPcmSize .* DCastInt (Get chanCnt)) , Direct DNullptr)
      p <%> contIf (Get ret .>= Value 0)
      p <%> update samplesDone (Direct $ Get samplesDone .+ DCastInt (Get ret))
      f' <- p <%> call HLib.PlusPtr (var fptr, Direct $ DCastInt (Get ret) .* DCastInt (Get chanCnt) .* DCastInt sampleBytes)
      p <%> update fptr (Direct $ Get f')
      return ()

gTagsParse :: Eff (BuildAASTG A C) sig m => m ()
gTagsParse = do
  tags  <- p <%> call (HLib.Malloc @OpusTags) ()
  ctnt  <- p <%> decl anything
  dat   <- p <%> call HLib.NewCBytes(var ctnt)
  len   <- p <%> call HLib.CBytesLen(var ctnt)
  ds'   <- p <%> decl (Direct $ DCastInt (Get len))
  p <%> call TagsParse (var tags, var dat, var ds')
  return ()
  where p = Building @A @C

gTagsParseNull :: Eff (BuildAASTG A C) sig m => m ()
gTagsParseNull = do
  tags  <- p <%> decl (Direct DNullptr)
  ctnt  <- p <%> decl anything
  dat   <- p <%> call HLib.NewCBytes(var ctnt)
  len   <- p <%> call HLib.CBytesLen(var ctnt)
  ds'   <- p <%> decl (Direct $ DCastInt (Get len))
  p <%> call TagsParse (var tags, var dat, var ds')
  return ()
  where p = Building @A @C

gPcmRaw :: Eff (BuildAASTG A C) sig m => PKey (Ptr OggOpusFile) -> PKey CInt -> m ()
gPcmRaw handle linkIndex = do
  p <%> call PcmTotal (var handle, var linkIndex)
  p <%> call RawTotal (var handle, var linkIndex)
  p <%> call PcmTell  (var handle)
  p <%> call RawTell  (var handle)
  return ()
  where p = Building @A @C

gMkPcmBuf :: Eff (BuildAASTG A C) sig m => m (PKey (Ptr Int16))
gMkPcmBuf = do
  p <%> call (HLib.MallocBytes @Int16) (value (sizeOf (0 :: Int16) * pcmSize))
  where p = Building @A @C

gReadStereo :: Eff (BuildAASTG A C) sig m => PKey (Ptr OggOpusFile) -> PKey (Ptr Int16) -> m ()
gReadStereo handle pcmBuf = do
  ret <- p <%> call ReadStereo (var handle, var pcmBuf, value pcmSize)
  p <%> contIf (Get ret .> Value 0)
  linkIx <- p <%> call CurrentLink (var handle)
  gPcmRaw handle linkIx
  p <%> call Tags (var handle, var linkIx)
  return ()
  where p = Building @A @C

gOpusFileFuzz :: Eff (BuildAASTG A C) sig m => PKey (Ptr OggOpusFile) -> m ()
gOpusFileFuzz handle = do
  p <%> call LinkCount (var handle)
  li <- p <%> val (-1)
  gPcmRaw handle li
  buf <- gMkPcmBuf
  p <%> while (Value True) (gReadStereo handle buf)
  where p = Building @A @C


useless1 :: Eff (BuildAASTG A C) sig m => m ()
useless1 = do
  p <%> val 'a' >> return ()
  where p = Building @A @C

useless2 :: Eff (BuildAASTG A C) sig m => m ()
useless2 = do
  p <%> val 'b' >> return ()
  where p = Building @A @C

useless3 :: Eff (BuildAASTG A C) sig m => m ()
useless3 = do
  p <%> val 'c' >> return ()
  where p = Building @A @C

useless4 :: Eff (BuildAASTG A C) sig m => m ()
useless4 = do
  p <%> decl (Direct (DNullptr @(Ptr OpusTags)))
  p <%> val 'd' >> return ()
  where p = Building @A @C

pcmSize :: Integral a => a
pcmSize = 120 * 48 * 2

instance TyConstC OggOpusFile where
  toCConst _ = undefined -- We do not contain constant OggOpusFile struct in the stub
  toCBType _ = CBNamed "OggOpusFile"

instance CStorable OpusTags
instance Storable OpusTags where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance TyConstC OpusTags where
  toCConst _ = undefined -- We do not contain constant OpusTags struct in the stub
  toCBType _ = CBNamed "OpusTags"

-- helper function
(<:>) :: Monad m => m a -> m [a] -> m [a]
a <:> b = do
  a' <- a
  b' <- b
  return (a' : b')

infixr 4 <:>
