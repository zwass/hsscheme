{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module Scheme where

import Control.Monad.Error

data Val = IntVal Int | FloatVal Float | StringVal String | BoolVal Bool
          deriving (Eq, Ord)

instance Show Val where
  show (IntVal x) = show x
  show (FloatVal x) = show x
  show (StringVal x) = show x
  show (BoolVal x) = show x

data Exp where
  Val :: Val -> Exp
  -- Expressions
  Plus :: Exp -> Exp -> Exp
  Minus :: Exp -> Exp -> Exp
  Times :: Exp -> Exp -> Exp
  Divide :: Exp -> Exp -> Exp

  If :: Exp -> Exp -> Exp -> Exp
  Equal :: Exp -> Exp -> Exp
  Geq :: Exp -> Exp -> Exp
  Gt :: Exp -> Exp -> Exp
  Leq :: Exp -> Exp -> Exp
  Lt :: Exp -> Exp -> Exp

instance Show Exp where
  -- show :: Exp -> String
  show (Val x) = show x
  show (Plus x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
  show (Minus x y) = "(- " ++ show x ++ " " ++ show y ++ ")"
  show (Times x y) = "(* " ++ show x ++ " " ++ show y ++ ")"
  show (Divide x y) = "(/ " ++ show x ++ " " ++ show y ++ ")"

  show (If p t e) = "(if " ++ show p ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Equal x y) = "(== " ++ show x ++ " " ++ show y ++ ")"
  show (Geq x y) = "(>= " ++ show x ++ " " ++ show y ++ ")"
  show (Gt x y) = "(> " ++ show x ++ " " ++ show y ++ ")"
  show (Leq x y) = "(<= " ++ show x ++ " " ++ show y ++ ")"
  show (Lt x y) = "(< " ++ show x ++ " " ++ show y ++ ")"

type Var = String

-- Used to build arithmetic operations
arith :: (Int -> Int -> Int) -> (Float -> Float -> Float)
      -> String -> Val -> Val -> Either String Val
arith o _  _ (IntVal x) (IntVal y) = return $ IntVal (x `o` y)
arith _ o _ (IntVal x) (FloatVal y) = return $ FloatVal ((fromIntegral x) `o` y)
arith _ o _ (FloatVal x) (IntVal y) = return $ FloatVal (x `o` (fromIntegral y))
arith _ o _ (FloatVal x) (FloatVal y) = return $ FloatVal (x `o` y)
arith _ _ s _ _ = throwError $ "Incompatible argument types in " ++ s

plus :: Val -> Val -> Either String Val
plus = arith (+) (+) "plus"

minus :: Val -> Val -> Either String Val
minus = arith (-) (-) "minus"

times :: Val -> Val -> Either String Val
times = arith (*) (*) "times"

-- Have to check div zero cases separately here
divide :: Val -> Val -> Either String Val
divide _ (IntVal 0) = throwError "Divide by zero"
divide _ (FloatVal 0) = throwError "Divide by zero"
divide x y = arith div (/) "div" x y

equal :: Val -> Val -> Val
equal (IntVal x) (IntVal y) = BoolVal (x == y)
equal (FloatVal x) (FloatVal y) = BoolVal (x == y)
equal (BoolVal x) (BoolVal y) = BoolVal (x == y)
equal (StringVal x) (StringVal y) = BoolVal (x == y)
equal _ _ = BoolVal False

leq :: Val -> Val -> Either String Val
leq (IntVal x) (IntVal y) = return $ BoolVal (x <= y)
leq (FloatVal x) (FloatVal y) = return $ BoolVal (x <= y)
leq (StringVal x) (StringVal y) = return $ BoolVal (x <= y)
leq _ _ = throwError "Incompatible argument types in leq"

lt :: Val -> Val -> Either String Val
lt (IntVal x) (IntVal y) = return $ BoolVal (x < y)
lt (FloatVal x) (FloatVal y) = return $ BoolVal (x < y)
lt (StringVal x) (StringVal y) = return $ BoolVal (x < y)
lt _ _ = throwError "Incompatible argument types in lt"

geq :: Val -> Val -> Either String Val
geq (IntVal x) (IntVal y) = return $ BoolVal (x >= y)
geq (FloatVal x) (FloatVal y) = return $ BoolVal (x >= y)
geq (StringVal x) (StringVal y) = return $ BoolVal (x >= y)
geq _ _ = throwError "Incompatible argument types in geq"

gt :: Val -> Val -> Either String Val
gt (IntVal x) (IntVal y) = return $ BoolVal (x > y)
gt (FloatVal x) (FloatVal y) = return $ BoolVal (x > y)
gt (StringVal x) (StringVal y) = return $ BoolVal (x > y)
gt _ _ = throwError "Incompatible argument types in gt"


eval :: Exp -> Either String Val
eval (Val x) = return x
eval (Plus x y) = do
  x' <- eval x
  y' <- eval y
  plus x' y'
eval (Minus x y) = do
  x' <- eval x
  y' <- eval y
  minus x' y'
eval (Times x y) = do
  x' <- eval x
  y' <- eval y
  times x' y'
eval (Divide x y) = do
  x' <- eval x
  y' <- eval y
  divide x' y'
eval (If p t e) = do
  p' <- eval p
  case p' of
    (BoolVal True) -> eval t
    (BoolVal False) -> eval e
    _ -> throwError "Incompatible argument types in if"
eval (Equal x y) = do
  x' <- eval x
  y' <- eval y
  return $ equal x' y'
eval (Geq x y) = do
  x' <- eval x
  y' <- eval y
  geq x' y'
eval (Gt x y) = do
  x' <- eval x
  y' <- eval y
  gt x' y'
eval (Leq x y) = do
  x' <- eval x
  y' <- eval y
  leq x' y'
eval (Lt x y) = do
  x' <- eval x
  y' <- eval y
  lt x' y'
