{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module LibFuzzer.Example where

import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS
import Test.HAPI
import Data.Data (Typeable)
import Control.Monad.IO.Class (liftIO)
import Test.HAPI.HLib.HLibPrelude (HLibPrelude)
import qualified Test.HAPI.HLib.HLibPrelude as HLib
import Test.HAPI.ApiTrace.CodeGen.C.DataType
import Test.HAPI.Constraint
import Data.Serialize (Serialize)

conduct :: LibFuzzerConduct
conduct = libFuzzerConductViaAASTG cograph

foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
  :: CString -> CSize -> IO CInt

testOneInputM = llvmFuzzerTestOneInputM conduct

main = mainM conduct

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

instance ApiName  ArithApi where
  apiNameUnder "C" Add = "broken_add"
  apiNameUnder "C" Sub = "segfault_minus"
  apiNameUnder "C" Mul = "stateful_multiply"
  apiNameUnder "C" Neg = "limited_input_range_negate"
  apiNameUnder _   a   = apiName a

instance Entry2BlockC ArithApi
instance HasForeignDef ArithApi where
  evalForeign Add = implE $ \a b -> fromIntegral <$> liftIO (add (fromIntegral a) (fromIntegral b))
  evalForeign Sub = implE $ \a b -> fromIntegral <$> liftIO (sub (fromIntegral a) (fromIntegral b))
  evalForeign Mul = implE $ \a b -> fromIntegral <$> liftIO (mul (fromIntegral a) (fromIntegral b))
  evalForeign Neg = implE $ \a   -> fromIntegral <$> liftIO (neg (fromIntegral a))

type A = ArithApi :$$: HLibPrelude
type C = HSerialize :<>: Fuzzable :<>: CCodeGen

graph1 :: AASTG A C
graph1 = runEnv $ runBuildAASTG $ do
  a <- p <%> val @Int 10
  b <- p <%> var @Int anything
  p <%> call Add (getVar a, getVar b)
  where p = Building @A @C

graph2 :: AASTG A C
graph2 = runEnv $ runBuildAASTG $ do
  a <- p <%> var anything
  b <- p <%> var anything
  p <%> call Add (getVar a, getVar b)
  where p = Building @A @C

graph3 :: AASTG A C
graph3 = runEnv $ runBuildAASTG $ do
  a <- p <%> var anything
  b <- p <%> var anything
  c <- p <%> call Add (getVar b, getVar a)
  p <%> call Add (getVar a, getVar c)
  where p = Building @A @C

graph4 :: AASTG A C
graph4 = runEnv $ runBuildAASTG $ do
  a <- p <%> var anything
  b <- p <%> var anything
  c <- p <%> call Add (getVar a, getVar b)
  d <- p <%> call Add (getVar a, getVar c)
  p <%> call Add (getVar c, getVar d)
  where p = Building @A @C

graph5 :: AASTG A C
graph5 = runEnv $ runBuildAASTG $ do
  a <- p <%> var anything
  b <- p <%> var anything
  c <- p <%> call Add (getVar a, getVar b)
  d <- p <%> call Sub (getVar a, getVar c)
  p <%> call Add (getVar c, getVar d)
  where p = Building @A @C

graph6 :: AASTG A C
graph6 = runEnv $ runBuildAASTG $ do
  a <- p <%> var anything
  b <- p <%> var anything
  c <- p <%> call Add (getVar a, getVar b)
  d <- p <%> call Add (getVar a, getVar c)
  fork p $ p <%> call Neg (irange (-42) 65535)
  fork p $ p <%> call (HLib.+) (getVar c, getVar c)
  p <%> call Mul (getVar a, getVar d)
  where p = Building @A @C

cograph :: AASTG A C
cograph = runEnv $ coalesceAASTGs 500 [graph1, graph2, graph3, graph4, graph5, graph6]
