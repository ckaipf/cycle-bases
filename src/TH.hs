{-# LANGUAGE DataKinds, TemplateHaskell, ExistentialQuantification #-}

module TH (
    ConversionFunctions (..),
    
    primes,
    firstNthPrimes,
    
    declareConversionFunctions,
    conversionFunctionsList
    
    ) where

import Language.Haskell.TH
import Data.FiniteField
import DePina
import qualified Data.Numbers.Primes as P

data ConversionFunctions = forall p. (Orthogonalizeable p) => CF (Integer -> p) 
  
convFunction :: Integer -> ExpQ
convFunction p = do 
    [e| CF ((\x -> fromInteger x ) :: Integer -> $(primeField p)) |]

mkConvFunctionDec :: Integer -> DecQ
mkConvFunctionDec p = do
    cF <- convFunction p
    let name = mkName $ "convFunction" ++ show p
    return $ FunD name [Clause [] (NormalB cF) []]
   
conversionFunctionsList :: [Integer] -> Q Exp
conversionFunctionsList x = return $ ListE $ map (\d -> VarE (mkName ("convFunction" ++ show d))) x

firstNthPrimes = 40
primes = take firstNthPrimes $ P.primes

declareConversionFunctions :: Q [Dec]
declareConversionFunctions = mapM mkConvFunctionDec primes
