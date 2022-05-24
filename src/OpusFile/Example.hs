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
{-# LANGUAGE MagicHash #-}

module OpusFile.Example where
import Test.HAPI
import Foreign.C
import Data.Data (Typeable)

-- import qualified Network.Curl.Easy as Curl
import Control.Monad.IO.Class (liftIO)
import Test.HAPI.HLib.HLibPrelude (HLibPrelude)
import Data.ByteString (ByteString)
import qualified Sound.OpusFile as OpusFile
import Foreign
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import GHC.Ptr (Ptr(..))

import qualified Test.HAPI.HLib.HLibPrelude as HLib



-- conduct :: LibFuzzerConduct
-- conduct = libFuzzerConductViaAASTG g1

foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
  :: CString -> CSize -> IO CInt

testOneInputM = _llvmFuzzerTestOneInputM g1

-- main = mainM conduct



data OpusFileApi :: ApiDefinition where
  OpenMemoryBS :: OpusFileApi '[ByteString]       (Either Int OpusFile.Handle)
  -- OpenMemory   :: OpusFileApi '[VPtr CChar, CInt] (Either Int OpusFile.Handle)
  Free         :: OpusFileApi '[OpusFile.Handle] ()
  ChannelCount :: OpusFileApi '[OpusFile.Handle] (Either Int OpusFile.Channels)
  PcmTotal     :: OpusFileApi '[OpusFile.Handle] (Either String Int)
  DecodeInt16  :: OpusFileApi '[OpusFile.Handle] (OpusFile.Pcm Int16)



deriving instance Typeable (OpusFileApi p a)
deriving instance Show     (OpusFileApi p a)
deriving instance Eq       (OpusFileApi p a)

instance ApiName      OpusFileApi
instance Entry2BlockC OpusFileApi

instance HasForeignDef OpusFileApi where
  evalForeign = \case
    OpenMemoryBS -> implE $ liftIO . OpusFile.openMemoryBS
    -- OpenMemory   -> implE $ \p c -> liftIO $ OpusFile.openMemory p c
    Free         -> implE $ liftIO . OpusFile.free
    ChannelCount -> implE $ return . OpusFile.channelCount
    PcmTotal     -> implE $ return . OpusFile.pcmTotal
    DecodeInt16  -> implE $ liftIO . OpusFile.decodeInt16

type A = OpusFileApi :$$: HLibPrelude

type C = Fuzzable

g1 :: AASTG A C
g1 = runEnv $ runBuildAASTG $ do
  path  <- p <%> var Anything
  efile <- p <%> call OpenMemoryBS (getVar path)
  file  <- p <%> call HLib.fromRight (getVar efile)
  -- pcm   <- p <%> call DecodeInt16 (getVar file)
  return ()
  where p = Building @A @C


deriving instance Generic OpusFile.Handle
instance Hashable OpusFile.Handle where
instance Serialize OpusFile.Handle where
deriving instance Generic (OpusFile.Pcm a)
-- instance Serialize (OpusFile.Pcm a) where

-- instance TyConst CExpr (CDeclSpec, CDeclr -> CDeclr)
