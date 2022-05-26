{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE TypeOperators #-}

module Consumer.Example where

import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS
import Test.HAPI
import Data.Data (Typeable)
import Control.Monad.IO.Class (liftIO)
import Test.HAPI.HLib.HLibPrelude (HLibPrelude)
import qualified Test.HAPI.HLib.HLibPrelude as HLib

foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
  :: CString -> CSize -> IO CInt

testOneInputM :: CString -> CSize -> IO CInt
testOneInputM str size = do
  bs <- BS.packCStringLen (str, fromIntegral size)
  -- runFuzzTest cograph bs
  return 0

foreign import ccall "broken_add"
  add :: CInt -> CInt -> IO CInt
foreign import ccall "segfault_minus"
  sub :: CInt -> CInt -> IO CInt
foreign import ccall "stateful_multiply"
  mul :: CInt -> CInt -> IO CInt
foreign import ccall "limited_input_range_negate"
  neg :: CInt -> IO CInt

data ArithApi :: ApiDefinition where
  Add :: ArithApi '[Int, Int] Int
  Sub :: ArithApi '[Int, Int] Int
  Mul :: ArithApi '[Int, Int] Int
  Neg :: ArithApi '[Int] Int

deriving instance Typeable (ArithApi p a)
deriving instance Show     (ArithApi p a)
deriving instance Eq       (ArithApi p a)
instance ApiName  ArithApi

instance HasForeignDef ArithApi where
  evalForeign Add [args|a b|] = fromIntegral <$> liftIO (add (fromIntegral a) (fromIntegral b))
  evalForeign Sub [args|a b|] = fromIntegral <$> liftIO (sub (fromIntegral a) (fromIntegral b))
  evalForeign Mul [args|a b|] = fromIntegral <$> liftIO (mul (fromIntegral a) (fromIntegral b))
  evalForeign Neg [args|a|]   = fromIntegral <$> liftIO (neg (fromIntegral a))

type A = ArithApi :$$: HLibPrelude

-- graph1 :: forall c. BasicSpec c => AASTG A c
-- graph1 = runEnv $ runBuildAASTG $ do
--   a <- p <%> val @Int 10
--   b <- p <%> var @Int anything
--   p <%> call Add (getVar a, getVar b)
--   where p = Building @A @c

-- graph2 :: forall c. BasicSpec c => AASTG A c
-- graph2 = runEnv $ runBuildAASTG $ do
--   a <- p <%> var anything
--   b <- p <%> var anything
--   p <%> call Add (getVar a, getVar b)
--   where p = Building @A @c

-- graph3 :: forall c. BasicSpec c => AASTG A c
-- graph3 = runEnv $ runBuildAASTG $ do
--   a <- p <%> var anything
--   b <- p <%> var anything
--   c <- p <%> call Add (getVar b, getVar a)
--   p <%> call Add (getVar a, getVar c)
--   where p = Building @A @c

-- graph4 :: forall c. BasicSpec c => AASTG A c
-- graph4 = runEnv $ runBuildAASTG $ do
--   a <- p <%> var anything
--   b <- p <%> var anything
--   c <- p <%> call Add (getVar a, getVar b)
--   d <- p <%> call Add (getVar a, getVar c)
--   p <%> call Add (getVar c, getVar d)
--   where p = Building @A @c

-- graph5 :: forall c. BasicSpec c => AASTG A c
-- graph5 = runEnv $ runBuildAASTG $ do
--   a <- p <%> var anything
--   b <- p <%> var anything
--   c <- p <%> call Add (getVar a, getVar b)
--   d <- p <%> call Sub (getVar a, getVar c)
--   p <%> call Add (getVar c, getVar d)
--   where p = Building @A @c

-- graph6 :: forall c. BasicSpec c => AASTG A c
-- graph6 = runEnv $ runBuildAASTG $ do
--   a <- p <%> var anything
--   b <- p <%> var anything
--   c <- p <%> call Add (getVar a, getVar b)
--   d <- p <%> call Add (getVar a, getVar c)
--   fork p $ p <%> call Neg (getVar c)
--   fork p $ p <%> call (HLib.+) (getVar c, getVar c)
--   p <%> call Mul (getVar a, getVar d)
--   where p = Building @A @c

-- cograph :: forall c. BasicSpec c => AASTG A c
-- cograph = runEnv $ coalesceAASTGs 500 [graph1, graph2, graph3, graph4, graph5, graph6]
