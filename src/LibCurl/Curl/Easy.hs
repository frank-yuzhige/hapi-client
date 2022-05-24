{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Easy
-- Copyright : (c) Galois Inc 2007-2009
-- License   :
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Haskell binding to the libcurl <http://curl.haxx.se/> \"easy\" API.
-- The \"easy\" API provides a higher-level, easy-to-get-started calling
-- interface to the library's wide range of features for interacting
-- with HTTP\/FTP\/etc servers.
--
--------------------------------------------------------------------
module LibCurl.Curl.Easy where
import Foreign.C
import Foreign
import LibCurl.Curl.Types



-- FFI decls


foreign import ccall
  "curl_version_num" curl_version_num :: IO CInt

foreign import ccall
  "curl_version_str" curl_version_str :: IO CString

foreign import ccall
  "curl/easy.h curl_global_init" curl_global_init_prim :: CInt -> IO CInt

foreign import ccall
  "curl/easy.h curl_global_cleanup" curl_global_cleanup :: IO ()

foreign import ccall
  "curl/easy.h curl_easy_init" easy_initialize :: IO CurlH

foreign import ccall
  "curl/easy.h curl_easy_perform" easy_perform_prim :: CurlH -> IO CInt

foreign import ccall
  "curl_easy_duphandle" easy_duphandle :: CurlH -> IO CurlH

foreign import ccall
  "curl_easy_reset" easy_reset :: CurlH -> IO ()

foreign import ccall
  "curl_easy_setopt_long" easy_setopt_long :: CurlH -> Int -> Long -> IO CInt

foreign import ccall
  "curl_easy_setopt_longlong" easy_setopt_llong :: CurlH -> Int -> LLong -> IO CInt

foreign import ccall
  "curl_easy_setopt_string" easy_setopt_string :: CurlH -> Int -> Ptr CChar -> IO CInt

foreign import ccall
  "curl_easy_setopt_ptr" easy_setopt_ptr :: CurlH -> Int -> Ptr a -> IO CInt

foreign import ccall
  "curl_easy_setopt_ptr" easy_setopt_fptr :: CurlH -> Int -> FunPtr a -> IO CInt

-- foreign import ccall
--   "curl_easy_setopt_ptr" easy_setopt_wfun :: CurlH -> Int -> FunPtr WriteFunction -> IO CInt

-- foreign import ccall
--   "curl_easy_setopt_ptr" easy_setopt_rfun :: CurlH -> Int -> FunPtr ReadFunctionPrim -> IO CInt



