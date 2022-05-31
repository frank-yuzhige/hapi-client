{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module LibCurl.Example where
import Test.HAPI
import Foreign.C (CInt, peekCString)
import Data.Data (Typeable)

-- import qualified Network.Curl.Easy as Curl
import Control.Monad.IO.Class (liftIO)
import Test.HAPI.HLib.HLibPrelude (HLibPrelude)
import LibCurl.Curl.Easy
import LibCurl.Curl.Types
import LibCurl.Curl.Opts
import LibCurl.Curl.Code

data LibCurlEasy :: ApiDefinition where
  Initialize        :: LibCurlEasy '[]                  CurlH
  Perform           :: LibCurlEasy '[CurlH]             CurlCode
  SetOpt            :: LibCurlEasy '[CurlH, CurlOption] CurlCode
  DupHandle         :: LibCurlEasy '[CurlH]             CurlH
  Reset             :: LibCurlEasy '[CurlH]             ()
  CurlGlobalInit    :: LibCurlEasy '[CInt]              CurlCode
  CurlGlobalCleanup :: LibCurlEasy '[]                  ()
  CurlVersionNumber :: LibCurlEasy '[]                  Int
  CurlVersionString :: LibCurlEasy '[]                  String


deriving instance Typeable (LibCurlEasy p a)
deriving instance Show     (LibCurlEasy p a)
deriving instance Eq       (LibCurlEasy p a)

instance ApiName      LibCurlEasy
instance Entry2BlockC LibCurlEasy

instance HasForeignDef LibCurlEasy where
  evalForeign Initialize        = implE $ liftIO easy_initialize
  -- evalForeign Perform           = implE $ \c -> liftIO $ easy_perform_prim c
  -- evalForeign SetOpt            = implE $ \c o -> liftIO $ easys c o
  -- evalForeign DupHandle         = implE $ \c -> liftIO $ Curl.duphandle c
  -- evalForeign Reset             = implE $ \c -> liftIO $ Curl.reset c
  -- evalForeign CurlGlobalInit    = implE $ \x -> liftIO $ Curl.curl_global_init x
  evalForeign CurlGlobalCleanup = implE $ liftIO curl_global_cleanup
  evalForeign CurlVersionNumber = implE $ liftIO $ fromIntegral <$> curl_version_num
  evalForeign CurlVersionString = implE $ liftIO $ peekCString =<< curl_version_str

type A = LibCurlEasy :$$: HLibPrelude

type C = Fuzzable :<>: CCodeGen


