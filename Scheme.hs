{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module Scheme where

import Control.Monad.Error

data Type = Int | Float | String | Bool

data Exp where
  Val :: Type -> Exp
  -- Types
  IntVal :: Int -> Exp
  FloatVal :: Float -> Exp
  StringVal :: String -> Exp
  BoolVal :: Bool -> Exp

  -- Expressions
  Plus :: Exp -> Exp -> Exp
  Minus :: Exp -> Exp -> Exp


instance Show Exp where
  -- show :: Exp -> String
  show (Plus x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
  show (Minus x y) = "(- " ++ show x ++ " " ++ show y ++ ")"
  show (IntVal x) = show x
  show (FloatVal x) = show x
  show (BoolVal x) = show x
  show (StringVal x) = show x

type Var = String

plus :: Exp -> Exp -> Either String Exp
plus (IntVal x) (IntVal y) = return $ IntVal (x + y)
plus (IntVal x) (FloatVal y) = return $ FloatVal ((fromIntegral x) + y)
plus (FloatVal x) (IntVal y) = return $ FloatVal (x + (fromIntegral y))
plus (FloatVal x) (FloatVal y) = return $ FloatVal (x + y)
plus _ _ = throwError "incompatible arguments to plus"

minus :: Exp -> Exp -> Either String Exp
minus (IntVal x) (IntVal y) = return $ IntVal (x - y)
minus (IntVal x) (FloatVal y) = return $ FloatVal ((fromIntegral x) - y)
minus (FloatVal x) (IntVal y) = return $ FloatVal (x - (fromIntegral y))
minus (FloatVal x) (FloatVal y) = return $ FloatVal (x - y)
minus _ _ = throwError "incompatible arguments to plus"


eval :: Exp -> Either String Exp
eval (IntVal x) = return $ IntVal x
eval (FloatVal x) = return $ FloatVal x
eval (BoolVal x) = return $ BoolVal x
eval (StringVal x) = return $ StringVal x
eval (Plus x y) = do
  x' <- eval x
  y' <- eval y
  plus x' y'
eval (Minus x y) = do
  x' <- eval x
  y' <- eval y
  minus x' y'
