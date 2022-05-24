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

module Stateless.Example where


import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS
import Test.HAPI
import Data.Data (Typeable)
import Control.Monad.IO.Class (liftIO)
import Test.HAPI.HLib.HLibPrelude
import qualified Test.HAPI.HLib.HLibPrelude as HLib
import Test.HAPI.Constraint

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

type C = Fuzzable :<>: CCodeGen

graph1 :: AASTG A C
graph1 = runEnv $ runBuildAASTG $ do
  a <- p <%> val @Int 10
  b <- p <%> var @Int Anything
  p <%> call Add (getVar a, getVar b)
  where p = Building @A @C


addComp :: AASTG A C
addComp = runEnv $ runBuildAASTG $ do
  s <- p <%> currNode
  a <- p <%> var @Int Anything
  b <- p <%> var @Int Anything
  x <- p <%> call Add (getVar a, getVar b)
  y <- p <%> call (HLib.+) (getVar a, getVar b)
  p <%> assertTrue (HLib.==) (getVar x, getVar y)
  s' <- p <%> currNode
  p <%(s', s)%> redirect
  where p = Building @A @C

addAssoc :: AASTG A C
addAssoc = runEnv $ runBuildAASTG $ do
  s <- p <%> currNode
  a <- p <%> var @Int Anything
  b <- p <%> var @Int Anything
  x <- p <%> call Add (getVar a, getVar b)
  y <- p <%> call Add (getVar b, getVar a)
  p <%> assertTrue (HLib.==) (getVar x, getVar y)
  s' <- p <%> currNode
  p <%(s', s)%> redirect
  where p = Building @A @C

mulAssoc :: AASTG A C
mulAssoc = runEnv $ runBuildAASTG $ do
  s <- p <%> currNode
  a <- p <%> var @Int Anything
  b <- p <%> var @Int Anything
  x <- p <%> call Mul (getVar a, getVar b)
  y <- p <%> call Mul (getVar b, getVar a)
  p <%> assertTrue (HLib.==) (getVar x, getVar y)
  -- s' <- p <%> currNode
  -- p <%(s', s)%> redirect
  where p = Building @A @C

cograph :: AASTG A C
cograph = runEnv $ coalesceAASTGs 500 [addAssoc,  addComp, mulAssoc]
