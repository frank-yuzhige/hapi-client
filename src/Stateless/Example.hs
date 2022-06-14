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
import Prelude hiding (div)

import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS
import Test.HAPI
import Data.Data (Typeable)
import Control.Monad.IO.Class (liftIO)
import qualified Test.HAPI.HLib.HLibPrelude as HLib
import Test.HAPI.Constraint
import Data.Serialize (Serialize)
import Test.HAPI.HLib.HLibPrelude (HLibPrelude)
import Test.HAPI.Conduct.Quickcheck (QuickCheckConduct, quickCheckConduct)

-- conduct :: LibFuzzerConduct
-- conduct = libFuzzerConductViaAASTG [] cograph

-- foreign export ccall "LLVMFuzzerTestOneInput" testOneInputM
--   :: CString -> CSize -> IO CInt

-- testOneInputM = llvmFuzzerTestOneInputM conduct

-- main = mainM conduct

foreign import ccall "broken_add"
  add :: CInt -> CInt -> IO CInt
foreign import ccall "segfault_minus"
  sub :: CInt -> CInt -> IO CInt
foreign import ccall "multiply"
  mul :: CInt -> CInt -> IO CInt
foreign import ccall "divide"
  div :: CInt -> CInt -> IO CInt
foreign import ccall "negate"
  neg :: CInt -> IO CInt

foreign import ccall "good_add"
  gadd :: CInt -> CInt -> IO CInt
foreign import ccall "good_minus"
  gsub :: CInt -> CInt -> IO CInt
foreign import ccall "good_multiply"
  gmul :: CInt -> CInt -> IO CInt
foreign import ccall "good_divide"
  gdiv :: CInt -> CInt -> IO CInt
foreign import ccall "good_negate"
  gneg :: CInt -> IO CInt

data ArithApi :: ApiDefinition where
  Add :: ArithApi '[CInt, CInt] CInt
  Sub :: ArithApi '[CInt, CInt] CInt
  Mul :: ArithApi '[CInt, CInt] CInt
  Div :: ArithApi '[CInt, CInt] CInt
  Neg :: ArithApi '[CInt] CInt

data GoodArithApi :: ApiDefinition where
  GAdd :: GoodArithApi '[CInt, CInt] CInt
  GSub :: GoodArithApi '[CInt, CInt] CInt
  GMul :: GoodArithApi '[CInt, CInt] CInt
  GDiv :: GoodArithApi '[CInt, CInt] CInt
  GNeg :: GoodArithApi '[CInt] CInt

deriving instance Typeable (ArithApi p a)
deriving instance Show     (ArithApi p a)
deriving instance Eq       (ArithApi p a)
deriving instance Typeable (GoodArithApi p a)
deriving instance Show     (GoodArithApi p a)
deriving instance Eq       (GoodArithApi p a)

instance ApiName  ArithApi where
  apiNameUnder "C" Add = "broken_add"
  apiNameUnder "C" Sub = "segfault_minus"
  apiNameUnder "C" Mul = "multiply"
  apiNameUnder "C" Div = "divide"
  apiNameUnder "C" Neg = "negate"
  apiNameUnder _   a   = apiName a

  apiArgsAreValid Div (_ ::* b ::* Nil) = if b /= 0 then Nothing else Just "div by zero"
  apiArgsAreValid Neg (a ::* Nil)
    | a < -42   = Just "Too small"
    | a > 65535 = Just "Too large"
    | otherwise = Nothing
  apiArgsAreValid _ _ = Nothing

instance Entry2BlockC ArithApi

instance ApiName  GoodArithApi where
  apiNameUnder "C" GAdd = "good_add"
  apiNameUnder "C" GSub = "good_add"
  apiNameUnder "C" GMul = "good_multiply"
  apiNameUnder "C" GDiv = "good_divide"
  apiNameUnder "C" GNeg = "good_negate"
  apiNameUnder _   a   = apiName a

  apiArgsAreValid GDiv (_ ::* b ::* Nil) = if b /= 0 then Nothing else Just "div by zero"
  apiArgsAreValid _ _ = Nothing

instance Entry2BlockC GoodArithApi

instance HasForeignDef ArithApi where
  evalForeign Add = implE $ \a b -> liftIO (add a b)
  evalForeign Sub = implE $ \a b -> liftIO (sub a b)
  evalForeign Mul = implE $ \a b -> liftIO (mul a b)
  evalForeign Div = implE $ \a b -> liftIO (div a b)
  evalForeign Neg = implE $ \a   -> liftIO (neg a)

instance HasForeignDef GoodArithApi where
  evalForeign GAdd = implE $ \a b -> liftIO (gadd a b)
  evalForeign GSub = implE $ \a b -> liftIO (gsub a b)
  evalForeign GMul = implE $ \a b -> liftIO (gmul a b)
  evalForeign GDiv = implE $ \a b -> liftIO (gdiv a b)
  evalForeign GNeg = implE $ \a   -> liftIO (gneg a)

type A = ArithApi :$$: GoodArithApi :$$: HLibPrelude
type C = Fuzzable :<>: Arbitrary :<>: HSerialize :<>: CCodeGen

intInRange :: Attribute C CInt
intInRange = range (-20000) 20000

addComp :: AASTG A C
addComp = runEnv $ runBuildAASTG $ do
  a <- p <%> decl intInRange
  b <- p <%> decl intInRange
  x <- p <%> call Add (var a, var b)
  y <- p <%> call GAdd (var a, var b)
  p <%> assertTrue (HLib.==) (var x, var y)
  where p = Building @A @C

subComp :: AASTG A C
subComp = runEnv $ runBuildAASTG $ do
  a <- p <%> decl intInRange
  b <- p <%> decl intInRange
  x <- p <%> call Sub (var a, var b)
  y <- p <%> call GSub (var a, var b)
  p <%> assertTrue (HLib.==) (var x, var y)
  where p = Building @A @C

mulComp :: AASTG A C
mulComp = runEnv $ runBuildAASTG $ do
  a <- p <%> decl intInRange
  b <- p <%> decl intInRange
  x <- p <%> call Mul (var a, var b)
  y <- p <%> call GMul (var a, var b)
  p <%> assertTrue (HLib.==) (var x, var y)
  where p = Building @A @C

divComp :: AASTG A C
divComp = runEnv $ runBuildAASTG $ do
  a <- p <%> decl intInRange
  b <- p <%> decl intInRange
  x <- p <%> call Div (var a, var b)
  y <- p <%> call GDiv (var a, var b)
  p <%> assertTrue (HLib.==) (var x, var y)
  where p = Building @A @C

negComp :: AASTG A C
negComp = runEnv $ runBuildAASTG $ do
  a <- p <%> decl intInRange
  x <- p <%> call Neg (var a)
  y <- p <%> call GNeg (var a)
  p <%> assertTrue (HLib.==) (var x, var y)
  where p = Building @A @C

cograph :: AASTG A C
cograph = runEnv $ coalesceAASTGs 500 [addComp, subComp, mulComp, divComp, negComp]
